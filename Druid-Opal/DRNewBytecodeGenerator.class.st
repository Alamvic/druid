Class {
	#name : 'DRNewBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'methodName',
		'numArgs',
		'vectorTempIndex',
		'generationStack',
		'opalBytecodeBuilder',
		'generationStacks',
		'alreadyGenerated'
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

	opalBytecodeBuilder createTempVectorNamed: vectorName withVars: temporariesList
]

{ #category : 'as yet unclassified' }
DRNewBytecodeGenerator >> ensureOperandsOnStack: aDRInstruction [
	
	| operandsToGenerate |
	operandsToGenerate := self instructionOperands: aDRInstruction.
	operandsToGenerate do: [ :e | self visitInstruction: e ].
	self reorderOperands: operandsToGenerate.
	^ operandsToGenerate
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> generateJump: aDRBranch [

	| createJumpToEnd trueLabel falseLabel |
	self flag: #TOCHECK. "Always ifTrue: ?"
	opalBytecodeBuilder addLiteral: #ifTrue:.

	trueLabel := self labelBlock: aDRBranch trueBranch.
	falseLabel := self labelBlock: aDRBranch falseBranch.
		
	opalBytecodeBuilder jumpAheadTo: falseLabel if: false.
	opalBytecodeBuilder jumpAheadTo: trueLabel.

]

{ #category : 'API' }
DRNewBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBasicBlock: block ]
]

{ #category : 'initialization' }
DRNewBytecodeGenerator >> initialize [

	opalBytecodeBuilder := OCIRBuilder new.
	numArgs := 0.
	
	generationStacks := Dictionary new.
	alreadyGenerated := Set new.
]

{ #category : 'as yet unclassified' }
DRNewBytecodeGenerator >> instructionOperands: aDRInstruction [

	(aDRInstruction isInstruction and: [ aDRInstruction isDRBlockClosure ])
		ifTrue: [ 
			"We do not need to push anything, that's done by the builder itself.
			Strange, huh?"
			^ #(  ) ].
	
	"Pops really have no operands. Why do they have operands?
	This is a leftover of a different Pop instruction from the Cogit"
	(aDRInstruction isInstruction and: [ aDRInstruction isPop ])
		ifTrue: [ ^ #(  ) ].
	
	"Load/Store i.v.s have different behavior depending on inlined or self accesses...
	Check the corresponding visit methods, they expect different stack layouts"
	(aDRInstruction isInstruction and: [ aDRInstruction isLoadInstanceVariable ]) ifTrue: [
			^ (self isSelf: aDRInstruction operand1)
				  ifTrue: [ #(  ) ]
				  ifFalse: [ aDRInstruction operands ] ].
	(aDRInstruction isInstruction and: [ aDRInstruction isStoreInstanceVariable ]) ifTrue: [
			^ (self isSelf: aDRInstruction operand1)
				  ifTrue: [ { aDRInstruction operands last } ]
				  ifFalse: [ aDRInstruction operands ] ].
	^ aDRInstruction operands
]

{ #category : 'API' }
DRNewBytecodeGenerator >> interpretInitializeScope: scope [

	| args  |	
	args := scope argumentNames.
	
	"Add arguments"
	args do: [ :arg| 
		opalBytecodeBuilder addTemp: arg ].
	
	"Add temps copied from outer"
	scope copiedVarNames do: [ :tmp| 
		opalBytecodeBuilder addTemp: tmp ].
		
	"Add own temp vectors (could be many due to inlinings)"
	scope definedTempVectors keysAndValuesDo: [ :vector :temps |
		self createTempVector: temps vectorName: vector
		 ].
	
	"Store arguments to temp vectors"
	args do: [ :arg | 
		opalBytecodeBuilder pushTemp: arg.
		opalBytecodeBuilder storeRemoteTemp: arg inVector: scope tempVectorName .
		opalBytecodeBuilder popTop ].
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
DRNewBytecodeGenerator >> reorderOperands: operands [

	"Maybe there is nothing to do"
	| operandStackMap stackDepthToReorder tempNames reorderedInstructions |
	(generationStack first: operands size) reversed = operands asArray
		ifTrue: [ ^ self ].
	
	operandStackMap := Dictionary new.
	operands do: [ :op |
		operandStackMap at: op put: (generationStack indexOf: op asLink ifAbsent: [ self error ]).
	].
	stackDepthToReorder := operandStackMap values max.
	1 to: stackDepthToReorder do: [ :i |
		operandStackMap at: (generationStack at: i) put: i.
	].
	tempNames := (1 to: stackDepthToReorder) collect: [ :idx | ('swap', idx asString) asSymbol ].
	self haltIf: [ stackDepthToReorder > 9 ].
	"Store all values into temps"
	opalBytecodeBuilder addTemps: tempNames.
	tempNames do: [ :n |
		opalBytecodeBuilder storeTemp: n.
		opalBytecodeBuilder popTop.
	].
	reorderedInstructions := (generationStack first: stackDepthToReorder) reversed.
	stackDepthToReorder timesRepeat: [ 
		self pop: generationStack top ].
	"Push all but the ones we care about"
	reorderedInstructions do: [ :i |
		(operands includes: i)
			ifFalse: [
				| index |
				index := operandStackMap at: i.
				opalBytecodeBuilder pushTemp: ('swap', index asString) asSymbol.
				self abstractPush: i.
		] ].
	"Finally push the ones we care about!"
	operands do: [ :i |
		| index |
		index := operandStackMap at: i.
		opalBytecodeBuilder pushTemp: ('swap', index asString) asSymbol.
		self abstractPush: i.
	].

	"Let's see if swapping fixed the order"
	self assert: (generationStack first: operands size) reversed = operands asArray
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> sendMessage: selector fromInstruction: aDRInstruction [

	| sendAST |

	"TODO: Avoid come back to the AST. Include super in the IR."
	sendAST := aDRInstruction originAST.
	(sendAST ifNotNil: [ sendAST superOf ])
		ifNil: [ opalBytecodeBuilder send: selector ]
		ifNotNil: [ :behavior | opalBytecodeBuilder send: selector toSuperOf: behavior ]
]

{ #category : 'building' }
DRNewBytecodeGenerator >> targetAST [

	| method ir |
	ir := opalBytecodeBuilder ir.
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
		opalBytecodeBuilder pushLiteralVariable: Array binding.
		opalBytecodeBuilder pushLiteral: aDRArrayNode operands size.
		opalBytecodeBuilder send: #new:.
		aDRArrayNode operands withIndexDo: [ :each :index |
			opalBytecodeBuilder pushDup.
			opalBytecodeBuilder pushLiteral: index.
			each acceptVisitor: self.
			opalBytecodeBuilder send: #at:put:.
			opalBytecodeBuilder popTop ].
		^ self ].

	opalBytecodeBuilder pushConsArray: aDRArrayNode operands size.
	
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitBasicBlock: aDRBasicBlock [

	aDRBasicBlock hasPredecessors ifTrue: [
			| label |
			"Somebody will jump to me, set a target"
			label := self labelBlock: aDRBasicBlock.
			aDRBasicBlock predecessors do: [ :pred |
					pred endInstruction isBackJump
						ifTrue: [ opalBytecodeBuilder jumpBackTarget: label ]
						ifFalse: [ opalBytecodeBuilder jumpAheadTarget: label ] ] ].

	"This is the tree graph algo.
	We only start from the roots of the graph, i.e., mandatory instructions with side effects.
	Then we visit depth first post order.
	We only visit the current instruction if all dependencies are visited
		- data dependencies
		- effect dependencies
	For each instruction,
	 - consume the operands from the stack.
	 - gen the instruction
	 - pop it if no more users
	"
	"We enter a basic block. Each block has its own stack.
	We should merge the stacks of the predecessors here, and validate that they are conherent: all same size"
	generationStack := aDRBasicBlock predecessors ifEmpty: [ Stack new ] ifNotEmpty: [
			                   (aDRBasicBlock predecessors size = 1 or: [
				                    aDRBasicBlock predecessors anySatisfy: [ :p | (generationStacks includesKey: p) not ] ])
				                   ifTrue: [
				                   (generationStacks at: (aDRBasicBlock predecessors detect: [ :p | generationStacks includesKey: p ])) copy ]
				                   ifFalse: [
						                   | predecessorStacks newStack |
						                   predecessorStacks := aDRBasicBlock predecessors collect: [ :p | generationStacks at: p ].
						                   self assert: predecessorStacks size = 2.
						                   self assert: predecessorStacks first size = predecessorStacks second size.
						                   newStack := Stack new.
						                   predecessorStacks first size > 0 ifTrue: [
								                   predecessorStacks first size to: 1 do: [ :i |
										                   | left right merged |
										                   left := predecessorStacks first at: i.
										                   right := predecessorStacks first at: i.
										                   merged := left = right
											                             ifTrue: [ left ]
											                             ifFalse: [ DRNullValue new ].
										                   newStack push: merged ] ].
						                   newStack ] ].
	generationStacks at: aDRBasicBlock put: generationStack.
	aDRBasicBlock instructionsDo: [ :i | i isMandatoryInstruction ifTrue: [ self visitInstruction: i ] ].
	"Remove it so we avoid stack confusion :)"
	generationStack := nil
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

	opalBytecodeBuilder
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

	aDRConstantValue value isLiteral ifFalse: [
			aDRConstantValue value isThisContextVariable
				ifTrue: [ ^ opalBytecodeBuilder pushThisContext ].
			aDRConstantValue value isGlobalVariable
				ifTrue: [ ^ opalBytecodeBuilder pushLiteralVariable: aDRConstantValue value ].
			(aDRConstantValue value isKindOf: DRClassType)
				ifTrue: [ ^ opalBytecodeBuilder pushLiteralVariable: aDRConstantValue value binding ] ].

	opalBytecodeBuilder pushLiteral: aDRConstantValue value
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitCopy: aDRCopy [

	"gen does nothing for a copy!"
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
DRNewBytecodeGenerator >> visitInstruction: aDRInstruction [

	| sizeBefore operands userSize |
	"Make constant values pushed every time instead of fetched from the stack"
	(aDRInstruction isInstruction and: [ alreadyGenerated includes: aDRInstruction ]) ifTrue: [ ^ self ].
	alreadyGenerated add: aDRInstruction.

	"Visit dependencies"
	operands := self ensureOperandsOnStack: aDRInstruction.

	sizeBefore := generationStack size.

	"Do the instruction's GEN"
	aDRInstruction acceptVisitor: self.

	"Pop this instruction value if this is an instruction, not a value"
	aDRInstruction isInstruction ifTrue: [
			| users |
			"Closures have a dependency to the value, but they do not need to be pushed,
		so they are actually not popped either"
			users := aDRInstruction users select: [ :u | u isDRBlockClosure not ].

			"Return, jump, initscope, do not push a result, no need to pop."
			(users size = 0 and: [
					 aDRInstruction isReturn not and: [
						 aDRInstruction isJump not and: [ (aDRInstruction class = DRInitializeScope) not and: [ aDRInstruction isPush not ] ] ] ])
				ifTrue: [ opalBytecodeBuilder popTop ].
			users size > 1 ifTrue: [ users size - 1 timesRepeat: [ opalBytecodeBuilder pushDup ] ] ].

	"Maintain abstract stack.
	First check nobody broke it.
	Then pop the operands if I'm the last user.
	Finally, push myself"
	self assert: sizeBefore = generationStack size.
	operands reversed do: [ :e | (e users allSatisfy: [ :u | alreadyGenerated includes: e ]) ifTrue: [ self pop: e ] ].

	userSize := aDRInstruction isInstruction
		ifTrue: [ aDRInstruction users size ]
		ifFalse: [ 1 ].
	
	userSize timesRepeat: [ self abstractPush: aDRInstruction ]
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitInterpreterValue: aDRInterpreterValue [ 

	self flag: #TODO. "It is always self ?"
	opalBytecodeBuilder pushReceiver
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitJump: aDRJump [

	| targetLabel |
	targetLabel := self labelBlock: aDRJump target.

	aDRJump isBackJump ifTrue: [
		^ opalBytecodeBuilder jumpBackTo: targetLabel ].

	^ opalBytecodeBuilder jumpAheadTo: targetLabel
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

	opalBytecodeBuilder pushRemoteTemp: aDRLoadArgument variableName inVector: aDRLoadArgument scope tempVectorName
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitLoadInstVar: aDRLoadInstVar [

	(self isSelf: aDRLoadInstVar operand1) ifTrue: [
			opalBytecodeBuilder pushInstVar: aDRLoadInstVar index.
			^ self ].

	opalBytecodeBuilder send: #instVarAt:
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLoadLiteralVariable: aDRLoadLiteralVariable [ 

	opalBytecodeBuilder pushLiteralVariable: aDRLoadLiteralVariable operand1 value
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitLoadTemporaryVariable: aDRLoadTemporaryVariable [

	opalBytecodeBuilder
		pushRemoteTemp: aDRLoadTemporaryVariable variableName value
		inVector: aDRLoadTemporaryVariable scope tempVectorName
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

	opalBytecodeBuilder pushLiteral: nil
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [

	self error: 'Phi functions are not compiled'
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitPhysicalRegister: aDRPhysicalRegister [

	self flag: #TODO. "Remove physical registers, use LoadReceiver / LoadArgument instructions"
	aDRPhysicalRegister name = 'ReceiverResultReg' ifTrue: [ ^ opalBytecodeBuilder pushReceiver ].
	1 halt
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitPop: aDRPop [

	"Does nothing"
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitPush: aDRPush [ 

	"gen does nothing for a push"
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitReturn: aDRReturn [

	opalBytecodeBuilder returnTop
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitStoreInstVar: aDRStoreInstanceVariable [

	(self isSelf: aDRStoreInstanceVariable operand1)
		ifTrue: [ opalBytecodeBuilder storeInstVar: aDRStoreInstanceVariable operand2 value ]
		ifFalse: [ opalBytecodeBuilder send: #instVarAt:put: ]
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitStoreLiteralVariable: aDRStoreLiteralVariable [

	opalBytecodeBuilder storeIntoLiteralVariable: aDRStoreLiteralVariable operand1 value
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	opalBytecodeBuilder
		storeRemoteTemp: aDRStoreTemporaryVariable variableName value
		inVector: aDRStoreTemporaryVariable scope tempVectorName
]

{ #category : 'ir-to-target' }
DRNewBytecodeGenerator >> visitSubstract: aDRSubstract [

	self sendMessage: #- fromInstruction: aDRSubstract
]

{ #category : 'visiting' }
DRNewBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	aDRBranchIfCondition operand1 acceptVisitor: self.
	opalBytecodeBuilder send: #class.
	opalBytecodeBuilder pushLiteralVariable: aDRBranchIfCondition operand2 value classType binding.
	opalBytecodeBuilder send: #=.
]
