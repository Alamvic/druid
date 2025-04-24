Class {
	#name : 'DRBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'builder',
		'methodName',
		'numArgs'
	],
	#category : 'Druid-BytecodeGeneration',
	#package : 'Druid',
	#tag : 'BytecodeGeneration'
}

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

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block |
		self visitBlock: block ]
]

{ #category : 'initialization' }
DRBytecodeGenerator >> initialize [

	builder := IRBuilder new.
	numArgs := 0
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
	1 to: anInteger do: [ :i | builder addTemp: 'arg' , (i - 1) asString ]
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

	| blockNode compiledBlock |
	blockNode := aDRBlockClosure blockNode.
	compiledBlock := OCASTTranslator new translateFullBlock: blockNode.
	builder
		pushFullClosureCompiledBlock: compiledBlock
		copiedValues: blockNode scope inComingCopiedVarNames
		"outerContextNeeded: blockNode hasNonLocalReturn"
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
	"	self visitBlock: aDRJump target"
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitLessOrEqualsThan: aDRLessOrEqualsThanComparison inBranch: aDRGetConditionCode [

	self sendMessage: #'<=' fromInstruction: aDRGetConditionCode
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [

	builder addTemp: aDRLoadArgument argName.
	builder pushTemp: aDRLoadArgument argName
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

	| value |
	value := aDRStoreTemporaryVariable operand2 simpleConstantFold asDRValue.
	value isStoreTemporaryVariable
		ifTrue: [ builder pushTemp: value operand1 value ]
		ifFalse: [ value acceptVisitor: self ].

	builder addTemp: aDRStoreTemporaryVariable operand1 value.
	builder storeTemp: aDRStoreTemporaryVariable operand1 value.
	builder popTop
]

{ #category : 'ir-to-target' }
DRBytecodeGenerator >> visitSubstract: aDRSubstract [

	self sendMessage: #- fromInstruction: aDRSubstract
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [

	| tempName |
	tempName := aDRLoadTemporaryVariable operand1 value.

	builder addTemp: tempName.
	builder pushTemp: tempName
]

{ #category : 'visiting' }
DRBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	aDRBranchIfCondition operand1 acceptVisitor: self.
	builder send: #class.
	builder pushLiteralVariable: aDRBranchIfCondition operand2 value classType binding.
	builder send: #=.
]
