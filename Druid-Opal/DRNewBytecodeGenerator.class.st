Class {
	#name : 'DRNewBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'builder',
		'methodName',
		'numArgs',
		'vectorTempIndex',
		'generationStack'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> abstractPush: aValue [

	generationStack push: aValue
]

{ #category : 'API' }
DRNewBytecodeGenerator >> createTempVector: temporariesList vectorName: vectorName [

	builder createTempVectorNamed: vectorName withVars: temporariesList
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> generateJump: aDRBranch [

	| createJumpToEnd trueLabel falseLabel |
	self flag: #TOCHECK. "Always ifTrue: ?"
	builder addLiteral: #ifTrue:.

	trueLabel := self labelBlock: aDRBranch trueBranch.
	falseLabel := self labelBlock: aDRBranch falseBranch.
		
	builder jumpAheadTo: falseLabel if: false.
	builder jumpAheadTo: trueLabel.

]

{ #category : 'API' }
DRNewBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'initialization' }
DRNewBytecodeGenerator >> initialize [

	builder := OCIRBuilder new.
	numArgs := 0.
	
	generationStack := Stack new.
]

{ #category : 'API' }
DRNewBytecodeGenerator >> interpretInitializeScope: scope [

	| args  |	
	args := scope argumentNames.
	
	"Add arguments"
	args do: [ :arg| 
		builder addTemp: arg ].
	
	"Add temps copied from outer"
	scope copiedVarNames do: [ :tmp| 
		builder addTemp: tmp ].
		
	"Add own temp vectors (could be many due to inlinings)"
	scope definedTempVectors keysAndValuesDo: [ :vector :temps |
		self createTempVector: temps vectorName: vector
		 ].
	
	"Store arguments to temp vectors"
	args do: [ :arg | 
		builder pushTemp: arg.
		builder storeRemoteTemp: arg inVector: scope tempVectorName .
		builder popTop ].
]

{ #category : 'testing' }
DRNewBytecodeGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [ aDRInstruction isStore ]
]

{ #category : 'testing' }
DRNewBytecodeGenerator >> isSelf: aDRInstruction [

	^ aDRInstruction simpleConstantFold asDRValue isInterpreterValue .
]

{ #category : 'jump' }
DRNewBytecodeGenerator >> labelBlock: aDRBasicBlock [

	^ ('b' , aDRBasicBlock id asString) asSymbol
]

{ #category : 'accessing' }
DRNewBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : 'accessing' }
DRNewBytecodeGenerator >> numArgs: anInteger [

	numArgs := anInteger.
]

{ #category : 'state stack' }
DRNewBytecodeGenerator >> pop: aDRInstruction [
	
	"The top should be the given DRInstruction. Otherwise fail"
	self assert: generationStack top == aDRInstruction description: 'The stack value is not what was expected'.
	^ generationStack pop
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> reorderOperands: aDRInstruction [

	"Maybe there is nothing to do"
	| operandStackMap stackDepthToReorder tempNames reorderedInstructions |
	(generationStack first: aDRInstruction operands size) reversed = aDRInstruction operands asArray
		ifTrue: [ ^ self ].
	
	operandStackMap := Dictionary new.
	aDRInstruction operands do: [ :op |
		operandStackMap at: op put: (generationStack indexOf: op asLink ifAbsent: [ self error ]).
	].
	stackDepthToReorder := operandStackMap values max.
	1 to: stackDepthToReorder do: [ :i |
		operandStackMap at: (generationStack at: i) put: i.
	].
	tempNames := (1 to: stackDepthToReorder) collect: [ :idx | ('swap', idx asString) asSymbol ].
	self haltIf: [ stackDepthToReorder > 3 ].
	"Store all values into temps"
	builder addTemps: tempNames.
	tempNames do: [ :n |
		builder storeTemp: n.
		builder popTop.
	].
	reorderedInstructions := (generationStack first: stackDepthToReorder) reversed.
	stackDepthToReorder timesRepeat: [ 
		self pop: generationStack top ].
	"Push all but the ones we care about"
	reorderedInstructions do: [ :i |
		(aDRInstruction operands includes: i)
			ifFalse: [
				| index |
				index := operandStackMap at: i.
				builder pushTemp: ('swap', index asString) asSymbol.
				self abstractPush: i.
		] ].
	"Finally push the ones we care about!"
	aDRInstruction operands do: [ :i |
		| index |
		index := operandStackMap at: i.
		builder pushTemp: ('swap', index asString) asSymbol.
		self abstractPush: i.
	].

	"Let's see if swapping fixed the order"
	self assert: (generationStack first: aDRInstruction operands size) reversed = aDRInstruction operands asArray
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> sendMessage: selector fromInstruction: aDRInstruction [

	| sendAST |
	
	aDRInstruction operands select: [ :o | o isInstruction not ] thenDo: [ :o | o acceptVisitor: self ].

	self reorderOperands: aDRInstruction.

	"TODO: Avoid come back to the AST. Include super in the IR."
	sendAST := aDRInstruction originAST.
	(sendAST ifNotNil: [ sendAST superOf ])
		ifNil: [ builder send: selector ]
		ifNotNil: [ :behavior | builder send: selector toSuperOf: behavior ].

	aDRInstruction operands reversed do: [ :op |
		self pop: op ].
	self abstractPush: aDRInstruction.

	aDRInstruction isConditionalBranch ifFalse: [
		aDRInstruction users ifEmpty: [ builder popTop ] ].
	aDRInstruction hasUsers	ifFalse: [ self pop: aDRInstruction ]

]

{ #category : 'building' }
DRNewBytecodeGenerator >> targetAST [

	| method ir |
	ir := builder ir.
	ir numArgs: numArgs.

	method := ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitAddition: aDRAdd [

	self sendMessage: #+ fromInstruction: aDRAdd
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitArrayNode: aDRArrayNode [

	aDRArrayNode operands size > 127 ifTrue: [ "Long form: using at:put:"
		1error.
		builder pushLiteralVariable: Array binding.
		builder pushLiteral: aDRArrayNode operands size.
		builder send: #new:.
		aDRArrayNode operands withIndexDo: [ :each :index |
			builder pushDup.
			builder pushLiteral: index.
			each acceptVisitor: self.
			builder send: #at:put:.
			builder popTop ].
		^ self ].

	aDRArrayNode operands do: [ :each | each acceptVisitor: self ].
	aDRArrayNode operands reversed do: [ :e | self pop: e ].

	self abstractPush: aDRArrayNode.
	builder pushConsArray: aDRArrayNode operands size.
	
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitBitAnd: aDRBitAnd [ 

	self sendMessage: #bitAnd: fromInstruction: aDRBitAnd 
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitBitOr: aDRBitOr [

	self sendMessage: #bitOr: fromInstruction: aDRBitOr
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitBitShift: aDRBitShift [ 

	self sendMessage: #bitShift: fromInstruction: aDRBitShift 
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitBlock: aDRBasicBlock [

	aDRBasicBlock hasPredecessors ifTrue: [
		| label |
		"Somebody will jump to me, set a target"
		label := self labelBlock: aDRBasicBlock.
		aDRBasicBlock predecessors do: [ :pred |
			pred endInstruction isBackJump
				ifTrue: [ builder jumpBackTarget: label ]
				ifFalse: [ builder jumpAheadTarget: label ] ] ].

	aDRBasicBlock instructionsDo: [ :i |	i acceptVisitor: self ]
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitBlockClosure: aDRBlockClosure [

	| compiledBlock blockCFG blockBytecodeGenerator copiedValues |

	blockCFG := aDRBlockClosure blockCFG.
	
	blockCFG applyOptimisation: DRFrameInfoCleaner new.
	blockCFG applyOptimisation: DRBranchCollapse new.
	blockCFG applyOptimisation: DRPhiSimplication new.
	blockCFG applyOptimisation: (DRSCCP then: DRDeadCodeElimination).
	DRLocalVariableInstructionScheluder new applyTo: blockCFG.
	blockCFG applyOptimisation: DRCleanScopes new.
	blockCFG applyOptimisation: DRDeadCodeElimination new.
	
	blockBytecodeGenerator := DRNewBytecodeBlockGenerator new.
	blockBytecodeGenerator blockScope: blockCFG scope.
	blockBytecodeGenerator generateTargetASTFromIR: blockCFG.	
	compiledBlock := blockBytecodeGenerator
		                 compileBlockFrom: blockCFG
		                 withScope: blockCFG scope.

	copiedValues := blockCFG scope copiedVarNames.

	self abstractPush: aDRBlockClosure.
	builder
		pushFullClosureCompiledBlock: compiledBlock
		copiedValues: copiedValues
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitBranchIfCondition: aDRConditionalBranch [

	aDRConditionalBranch condition
		acceptVisitor: self
		withBranch: aDRConditionalBranch.

	self generateJump: aDRConditionalBranch
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitConstant: aDRConstantValue [

	"Push the constant to the internal stack to track when it was pushed"
	self abstractPush: aDRConstantValue.
	
	aDRConstantValue value isLiteral ifFalse: [
		aDRConstantValue value isThisContextVariable ifTrue: [
			^ builder pushThisContext ].
		aDRConstantValue value isGlobalVariable ifTrue: [
			^ builder pushLiteralVariable: aDRConstantValue value ] ].

	builder pushLiteral: aDRConstantValue value
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitCopy: aDRCopy [

	aDRCopy operand1 isInstruction ifFalse: [ aDRCopy operand1 acceptVisitor: self ].
	self pop: aDRCopy operand1.
	self abstractPush: aDRCopy
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitDivision: aDRDivision [ 
	"TODO: / and // are compiled as this instruction"
	| selector |
	selector := 	aDRDivision originAST ifNotNil: [ :ast | ast selector ] ifNil: [ #/ ].
	self sendMessage: selector fromInstruction: aDRDivision 
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #= fromInstruction: aDRBranchIfCondition
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitGetConditionCode: aDRGetConditionCode [ 

	aDRGetConditionCode condition acceptVisitor: self withBranch: aDRGetConditionCode
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitGreaterOrEqualsThan: aDRGreaterOrEqualsThanComparison inBranch: aDRBranch [

	self sendMessage: #'>=' fromInstruction: aDRBranch
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitGreaterThan: aDRGreaterThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #> fromInstruction: aDRBranchIfCondition
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitInitializeScope: aDRInitializeScope [ 

	self interpretInitializeScope: aDRInitializeScope scope
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitInterpreterValue: aDRInterpreterValue [ 

	self flag: #TODO. "It is always self ?"
	self abstractPush: aDRInterpreterValue.
	builder pushReceiver
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitJump: aDRJump [

	| targetLabel |
	targetLabel := self labelBlock: aDRJump target.

	aDRJump isBackJump ifTrue: [
		^ builder jumpBackTo: targetLabel ].

	^ builder jumpAheadTo: targetLabel
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLessOrEqualsThan: aDRLessOrEqualsThanComparison inBranch: aDRGetConditionCode [

	self sendMessage: #'<=' fromInstruction: aDRGetConditionCode
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLessThan: aDRLessThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #< fromInstruction: aDRBranchIfCondition
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [

	self abstractPush: aDRLoadArgument.
	builder pushRemoteTemp: aDRLoadArgument variableName inVector: aDRLoadArgument scope tempVectorName. 
	
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitLoadInstVar: aDRLoadInstVar [
	
	self abstractPush: aDRLoadInstVar.
	(self isSelf: aDRLoadInstVar operand1) ifTrue: [
		builder pushInstVar: aDRLoadInstVar index.
		^ self 
	].

	aDRLoadInstVar operand1 isInstruction ifFalse: [ aDRLoadInstVar operand1 acceptVisitor: self ].
	builder pushLiteral: aDRLoadInstVar index.
	builder send: #instVarAt:.
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLoadLiteralVariable: aDRLoadLiteralVariable [ 

	self abstractPush: aDRLoadLiteralVariable.
	builder pushLiteralVariable: aDRLoadLiteralVariable operand1 value
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLoadTemporaryVariable: aDRLoadTemporaryVariable [

	self abstractPush: aDRLoadTemporaryVariable.
	builder pushRemoteTemp: aDRLoadTemporaryVariable operand1 value inVector: aDRLoadTemporaryVariable scope tempVectorName
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitMessageSend: aDRMessageSend [ 

	self sendMessage: aDRMessageSend selector fromInstruction: aDRMessageSend
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitMod: aDRMod [

	self sendMessage: #\\ fromInstruction: aDRMod
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitMultiply: aDRMultiply [

	self sendMessage: #* fromInstruction: aDRMultiply
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitNegate: aDRNegate [ 

	self sendMessage: #negated fromInstruction: aDRNegate
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitNoop: aDRNoop [

	"Do nothing"

	
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitNullValue: aDRNullValue [

	self abstractPush: aDRNullValue.
	builder pushLiteral: nil
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [

	self error: 'Phi functions are not compiled'
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitPhysicalRegister: aDRPhysicalRegister [

	self flag: #TODO. "Remove physical registers, use LoadReceiver / LoadArgument instructions"
	aDRPhysicalRegister name = 'ReceiverResultReg' ifTrue: [ ^ builder pushReceiver ].
	1 halt
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitPop: aDRPop [ 

	self abstractPush: aDRPop
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitPush: aDRPush [ 

	"Values are pushed on visiting, do nothing here"
	aDRPush operand1 isInstruction ifFalse: [ aDRPush operand1 acceptVisitor: self]
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 isInstruction ifFalse: [ 
		1halt.aDRReturn operand1 acceptVisitor: self ].

	self pop: aDRReturn operand1.
	builder returnTop.
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitStoreInstVar: aDRStoreInstanceVariable [

	(self isSelf: aDRStoreInstanceVariable operand1) ifTrue: [
			aDRStoreInstanceVariable operand3 isInstruction ifFalse: [ aDRStoreInstanceVariable operand3 acceptVisitor: self ].
			builder storeInstVar: aDRStoreInstanceVariable operand2 value.

			aDRStoreInstanceVariable users isEmpty ifTrue: [
					self pop: aDRStoreInstanceVariable operand3.
					builder popTop ].
			^ self ].

	aDRStoreInstanceVariable operand1 isInstruction ifFalse: [ aDRStoreInstanceVariable operand1 acceptVisitor: self ].
	builder pushLiteral: aDRStoreInstanceVariable operand2 value.
	aDRStoreInstanceVariable operand3 isInstruction ifFalse: [ aDRStoreInstanceVariable operand3 acceptVisitor: self ].
	builder send: #instVarAt:put:.


	self pop: aDRStoreInstanceVariable operands last.
	self pop: aDRStoreInstanceVariable operands first.
	self assert: aDRStoreInstanceVariable hasUsers not.
	builder popTop
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitStoreLiteralVariable: aDRStoreLiteralVariable [

	| value |
	value := aDRStoreLiteralVariable operand2.
	value acceptVisitor: self.

	builder storeIntoLiteralVariable:
		aDRStoreLiteralVariable operand1 value.

	aDRStoreLiteralVariable hasUsers ifFalse: [ builder popTop ]
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	| value |
	value := aDRStoreTemporaryVariable operand2 asDRValue.
	"Avoid store of stores, they should be compiled as store + read"	
	self assert: value isStoreTemporaryVariable not.
	
	value isInstruction ifFalse: [ value acceptVisitor: self ].

	builder
		storeRemoteTemp: aDRStoreTemporaryVariable operand1 value
		inVector: aDRStoreTemporaryVariable scope tempVectorName.

	"Stores should not have users, only block closures catch temporaries"	
	self assert: (aDRStoreTemporaryVariable users allSatisfy: [ :each | each isDRBlockClosure ]).		

	self pop: value.
	builder popTop

]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitSubstract: aDRSubstract [

	self sendMessage: #- fromInstruction: aDRSubstract
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	aDRBranchIfCondition operand1 acceptVisitor: self.
	builder send: #class.
	builder pushLiteralVariable: aDRBranchIfCondition operand2 value classType binding.
	builder send: #=.
]
