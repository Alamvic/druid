Class {
	#name : 'DRBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'builder',
		'methodName',
		'numArgs',
		'vectorTempIndex'
	],
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

{ #category : 'API' }
DRBytecodeGenerator >> createTempVector: temporariesList [
	
	temporariesList isNotEmpty ifTrue: [
		builder createTempVectorNamed: self vectorTempName withVars: temporariesList.]
	
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> generateJump: aDRBranch [

	| createJumpToEnd trueLabel falseLabel |
	self flag: #TOCHECK. "Always ifTrue: ?"
	builder addLiteral: #ifTrue:.

	trueLabel := self labelBlock: aDRBranch trueBranch.
	falseLabel := self labelBlock: aDRBranch falseBranch.
		
	builder jumpAheadTo: falseLabel if: false.
	builder jumpAheadTo: trueLabel.

]

{ #category : 'API' }
DRBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [
	
	self handleTemporaryVariableFromCFG: aDRControlFlowGraph .

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'API' }
DRBytecodeGenerator >> handleTemporaryVariableFromCFG: aDRControlFlowGraph [

	| args temps|
	
	"Create a tempVector with arguments and all temporary variable
	- Copy all arguments to the temp vector"
	args := aDRControlFlowGraph argumentNames.
	temps := aDRControlFlowGraph temporaryVariableNames. 
	
	args do: [ :arg| 
		builder addTemp: arg ].
	
	self createTempVector: args , temps.
	
	args do: [ :arg | 
		builder pushTemp: arg.
		builder storeRemoteTemp: arg inVector: self vectorTempName.
		builder popTop ].
]

{ #category : 'initialization' }
DRBytecodeGenerator >> initialize [

	builder := IRBuilder new.
	numArgs := 0.
	
	vectorTempIndex:= nil.
]

{ #category : 'testing' }
DRBytecodeGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [ aDRInstruction isStore ]
]

{ #category : 'testing' }
DRBytecodeGenerator >> isSelf: aDRInstruction [

	^ aDRInstruction simpleConstantFold asDRValue isInterpreterValue .
]

{ #category : 'jump' }
DRBytecodeGenerator >> labelBlock: aDRBasicBlock [

	^ ('b' , aDRBasicBlock id asString) asSymbol
]

{ #category : 'accessing' }
DRBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : 'accessing' }
DRBytecodeGenerator >> numArgs: anInteger [

	numArgs := anInteger.
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> sendMessage: selector fromInstruction: aDRInstruction [

	aDRInstruction operands do: [ :o | o acceptVisitor: self ].
	builder send: selector.
	
	aDRInstruction isConditionalBranch ifFalse: 
		[ aDRInstruction users ifEmpty: [ 
			builder popTop
			 ]]
]

{ #category : 'building' }
DRBytecodeGenerator >> targetAST [

	| method ir |
	ir := builder ir.
	ir numArgs: numArgs.

	method := ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : 'accessing' }
DRBytecodeGenerator >> vectorTempIndex [

	^ vectorTempIndex ifNil: [ ^ 0 ]
]

{ #category : 'accessing' }
DRBytecodeGenerator >> vectorTempIndex: anInteger [

	vectorTempIndex := anInteger
]

{ #category : 'accessing' }
DRBytecodeGenerator >> vectorTempName [

	"hardcoded for now, name comes from the default scope, and we use it for now"
	^ '0vector0' ", self vectorTempIndex asString"
]

{ #category : 'visiting' }
DRBytecodeGenerator >> vectorTempNameFromTempVar: aTemporaryVariable [

	^ self vectorTempName 
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitAddition: aDRAdd [

	self sendMessage: #+ fromInstruction: aDRAdd
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitBlock: aDRBasicBlock [

	aDRBasicBlock hasPredecessors ifTrue: [
		| label |
		"Somebody will jump to me, set a target"
		label := self labelBlock: aDRBasicBlock.
		aDRBasicBlock predecessors do: [ :pred |
			pred endInstruction isBackJump
				ifTrue: [ builder jumpBackTarget: label ]
				ifFalse: [ builder jumpAheadTarget: label ] ] ].

	aDRBasicBlock instructionsDo: [ :i |
		(self isResultInstruction: i) ifTrue: [ i acceptVisitor: self ] ]
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitBlockClosure: aDRBlockClosure [

	| blockNode compiledBlock blockCFG blockBuilder copiedValues |
	blockNode := aDRBlockClosure blockNode.
	blockCFG := aDRBlockClosure generateCFG.
	
	blockCFG applyOptimisation: DRFrameInfoCleaner new.
	blockCFG applyOptimisation: (DRBranchCollapse then: DRPhiSimplication).
	blockCFG applyOptimisation: (DRSCCP then: DRDeadCodeElimination).
	DRLocalVariableInstructionScheluder new applyTo: blockCFG.

	blockBuilder := DRBytecodeBlockGenerator new.
	blockBuilder generateTargetASTFromIR: blockCFG.
	
	compiledBlock := blockBuilder compileBlockFrom: blockCFG withScope: blockNode scope.
	
	copiedValues := blockCFG vectorTemporaryVariableNames isEmpty ifTrue: [ { } ]
		ifFalse: [ {self vectorTempName} ].
	builder
		pushFullClosureCompiledBlock: compiledBlock
		copiedValues: copiedValues 
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitBlockReturn: aDBlockRReturn [ 

	aDBlockRReturn operand1 acceptVisitor: self.
	builder blockReturnTop.
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitBranchIfCondition: aDRConditionalBranch [

	aDRConditionalBranch condition
		acceptVisitor: self
		withBranch: aDRConditionalBranch.

	self generateJump: aDRConditionalBranch
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitConstant: aDRConstantValue [

	aDRConstantValue value isLiteral ifFalse: [
		aDRConstantValue value isGlobalVariable ifTrue: [
			^ builder pushLiteralVariable: aDRConstantValue value ] ].

	builder pushLiteral: aDRConstantValue value
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitCopy: aDRCopy [

	aDRCopy operand1 acceptVisitor: self
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #= fromInstruction: aDRBranchIfCondition
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitGetConditionCode: aDRGetConditionCode [ 

	aDRGetConditionCode condition acceptVisitor: self withBranch: aDRGetConditionCode
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitInterpreterValue: aDRInterpreterValue [ 

	self flag: #TODO. "It is always self ?"
	builder pushReceiver
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitJump: aDRJump [

	| targetLabel |
	targetLabel := self labelBlock: aDRJump target.

	aDRJump isBackJump ifTrue: [
		^ builder jumpBackTo: targetLabel ].

	^ builder jumpAheadTo: targetLabel
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitLessOrEqualsThan: aDRLessOrEqualsThanComparison inBranch: aDRGetConditionCode [

	self sendMessage: #'<=' fromInstruction: aDRGetConditionCode
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [
	
	builder pushRemoteTemp: aDRLoadArgument argumentName inVector: self vectorTempName
	
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitLoadInstVar: aDRLoadInstVar [
	
	(self isSelf: aDRLoadInstVar operand1) ifTrue: [
		builder pushInstVar: aDRLoadInstVar index.
		^ self 
	].

	aDRLoadInstVar operand1 acceptVisitor: self.
	builder pushLiteral: aDRLoadInstVar index.
	builder send: #instVarAt:.
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitMessageSend: aDRMessageSend [ 

	self sendMessage: aDRMessageSend selector fromInstruction: aDRMessageSend
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitMultiply: aDRMultiply [

	self sendMessage: #* fromInstruction: aDRMultiply
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitNoop: aDRNoop [

	"Do nothing"

	
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitNullValue: aDRNullValue [

	builder pushLiteral: nil
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [

	self error: 'Phi functions are not compiled'
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitPhysicalRegister: aDRPhysicalRegister [

	self flag: #TODO. "Remove physical registers, use LoadReceiver / LoadArgument instructions"
	aDRPhysicalRegister name = 'ReceiverResultReg' ifTrue: [ ^ builder pushReceiver ].
	1 halt
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder returnTop.
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitStoreInstVar: aDRStoreInstanceVariable [
	
	(self isSelf: aDRStoreInstanceVariable operand1) ifTrue: [
		aDRStoreInstanceVariable operand3 acceptVisitor: self.
		builder storeInstVar: aDRStoreInstanceVariable operand2 value.
		aDRStoreInstanceVariable users size = 0 ifTrue: [ 
			builder popTop.
		].
		^ self 
	].
	aDRStoreInstanceVariable operand1 acceptVisitor: self.
	builder pushLiteral: aDRStoreInstanceVariable operand2 value.
	aDRStoreInstanceVariable operand3 acceptVisitor: self.
	builder send: #instVarAt:put:.
	builder popTop
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	| value vectorTempName |
	value := aDRStoreTemporaryVariable operand2 simpleConstantFold
		         asDRValue.
	value isStoreTemporaryVariable ifFalse: [ value acceptVisitor: self ].
	
	vectorTempName := self vectorTempNameFromTempVar:
		                  aDRStoreTemporaryVariable.

	builder
		storeRemoteTemp: aDRStoreTemporaryVariable operand1 value
		inVector: vectorTempName.

	(aDRStoreTemporaryVariable users reject: [ :each |
		 each isLoadTemporaryVariable ]) size = 0 ifTrue: [ builder popTop ]
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitSubstract: aDRSubstract [

	self sendMessage: #- fromInstruction: aDRSubstract
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [

	| vectorTempName |
	vectorTempName := self vectorTempNameFromTempVar:
		                  aDRLoadTemporaryVariable.

	builder
		pushRemoteTemp: aDRLoadTemporaryVariable operand1 value
		inVector: vectorTempName
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	aDRBranchIfCondition operand1 acceptVisitor: self.
	builder send: #class.
	builder pushLiteralVariable: aDRBranchIfCondition operand2 value classType binding.
	builder send: #=.
]
