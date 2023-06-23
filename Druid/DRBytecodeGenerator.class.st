Class {
	#name : #DRBytecodeGenerator,
	#superclass : #Object,
	#instVars : [
		'builder',
		'methodName',
		'numArgs',
		'currentBasicBlock',
		'visitedBlocks'
	],
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> generateJump: aDRBranch [

	| createJumpToEnd |
	self flag: #TOCHECK. "Always ifTrue: ?"
	builder addLiteral: #ifTrue:.

	builder jumpAheadTo: #else if: false.
	self visitBlock: aDRBranch trueBranch.
	createJumpToEnd := currentBasicBlock endInstruction isBackJump not.
	createJumpToEnd ifTrue: [ builder jumpAheadTo: #end ].
	builder jumpAheadTarget: #else.
	self visitBlock: aDRBranch falseBranch.
	createJumpToEnd ifTrue: [ builder jumpAheadTarget: #end ]
]

{ #category : #API }
DRBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	self visitBlock: aDRControlFlowGraph initialBasicBlock.
]

{ #category : #initialization }
DRBytecodeGenerator >> initialize [

	builder := IRBuilder new.
	numArgs := 0.
	visitedBlocks := Stack new
]

{ #category : #testing }
DRBytecodeGenerator >> isResultInstruction: aDRInstruction [

	^ aDRInstruction users isEmpty or: [ aDRInstruction isStore ]
]

{ #category : #accessing }
DRBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : #accessing }
DRBytecodeGenerator >> numArgs: anInteger [

	numArgs := anInteger.
	1 to: anInteger do: [ :i | builder addTemp: 'arg' , (i - 1) asString ]
]

{ #category : #'stack - blocks' }
DRBytecodeGenerator >> popBlock [

	^ visitedBlocks pop
]

{ #category : #'stack - blocks' }
DRBytecodeGenerator >> pushBlock: aDRBasicBlock [ 

	visitedBlocks push: aDRBasicBlock 
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> sendMessage: selector fromInstruction: aDRInstruction [

	aDRInstruction operands do: [ :o | o acceptVisitor: self ].
	builder send: selector.
	aDRInstruction hasUsers ifFalse: [
		builder popTop ]
]

{ #category : #building }
DRBytecodeGenerator >> targetAST [

	| method ir |
	ir := builder ir.
	ir numArgs: numArgs.

	method := ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitAddition: aDRAdd [

	self sendMessage: #+ fromInstruction: aDRAdd
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBlock: aDRBasicBlock [

	currentBasicBlock := aDRBasicBlock.
	self pushBlock: aDRBasicBlock.

	"Take care of backjumps!"
	(aDRBasicBlock predecessors anySatisfy: [ :pred |
		 pred endInstruction isBackJump ]) ifTrue: [
		builder jumpBackTarget: #begin ].

	aDRBasicBlock instructionsDo: [ :i |
		(self isResultInstruction: i) ifTrue: [ i acceptVisitor: self ] ].
	self assert: self popBlock = aDRBasicBlock
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBlockClosure: aDRBlockClosure [ 

	| blockCFG |
	blockCFG := aDRBlockClosure generateCFG. 1halt.
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBranchIfCondition: aDRConditionalBranch [

	aDRConditionalBranch condition
		acceptVisitor: self
		withBranch: aDRConditionalBranch.

	self generateJump: aDRConditionalBranch
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitConstant: aDRConstantValue [

	aDRConstantValue value isLiteral ifFalse: [
		aDRConstantValue value isGlobalVariable ifTrue: [
			^ builder pushLiteralVariable: aDRConstantValue value ] ].

	builder pushLiteral: aDRConstantValue value
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitCopy: aDRCopy [

	aDRCopy operand1 acceptVisitor: self
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #= fromInstruction: aDRBranchIfCondition
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitGetConditionCode: aDRGetConditionCode [ 

	aDRGetConditionCode condition acceptVisitor: self withBranch: aDRGetConditionCode
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitInterpreterValue: aDRInterpreterValue [ 

	self flag: #TODO. "It is always self ?"
	builder pushReceiver
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitJump: aDRJump [

	aDRJump isBackJump ifTrue: [ ^ builder jumpBackTo: #begin ].

	self visitBlock: aDRJump target
]

{ #category : #visiting }
DRBytecodeGenerator >> visitLessOrEqualsThan: aDRLessOrEqualsThanComparison inBranch: aDRGetConditionCode [

	self sendMessage: #'<=' fromInstruction: aDRGetConditionCode
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [

	builder addTemp: aDRLoadArgument argName.
	builder pushTemp: aDRLoadArgument argName
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitLoadInstVar: aDRLoadInstVar [ 
	
	aDRLoadInstVar operand1 acceptVisitor: self.
	builder pushLiteral: aDRLoadInstVar index.
	builder send: #instVarAt:
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitMessageSend: aDRMessageSend [ 

	self sendMessage: aDRMessageSend selector fromInstruction: aDRMessageSend
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitMultiply: aDRMultiply [

	self sendMessage: #* fromInstruction: aDRMultiply
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitNoop: aDRNoop [

	"Do nothing"

	
]

{ #category : #visiting }
DRBytecodeGenerator >> visitNullValue: aDRNullValue [

	builder pushLiteral: nil
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [

	| resolved |
	resolved := aDRPhiFunction valuesForPredecessor: (self visitedPredecessorOf: aDRPhiFunction basicBlock).
	resolved ifEmpty: [ 1halt ].
	resolved unique isStore ifTrue: [ ^ self ].
	resolved unique acceptVisitor: self
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitPhysicalRegister: aDRPhysicalRegister [

	self flag: #TODO. "Remove physical registers, use LoadReceiver / LoadArgument instructions"
	aDRPhysicalRegister name = 'ReceiverResultReg' ifTrue: [ ^ builder pushReceiver ].
	1 halt
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder returnTop.
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitStoreInstVar: aDRStoreInstanceVariable [ 

	aDRStoreInstanceVariable operand1 acceptVisitor: self.
	builder pushLiteral: aDRStoreInstanceVariable operand2 value.
	aDRStoreInstanceVariable operand3 acceptVisitor: self.
	builder send: #instVarAt:put:
]

{ #category : #visiting }
DRBytecodeGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	aDRStoreTemporaryVariable operand2 acceptVisitor: self.

	builder addTemp: aDRStoreTemporaryVariable operand1 value.
	builder storeTemp: aDRStoreTemporaryVariable operand1 value.
	builder popTop
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitSubstract: aDRSubstract [

	self sendMessage: #- fromInstruction: aDRSubstract
]

{ #category : #visiting }
DRBytecodeGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [

	| tempName |
	tempName := aDRLoadTemporaryVariable operand1 value.

	builder addTemp: tempName.
	builder pushTemp: tempName
]

{ #category : #visiting }
DRBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	aDRBranchIfCondition operand1 acceptVisitor: self.
	builder send: #class.
	builder pushLiteralVariable: aDRBranchIfCondition operand2 value classType binding.
	builder send: #=.
]

{ #category : #'stack - blocks' }
DRBytecodeGenerator >> visitedPredecessorOf: aDRBasicBlock [

	| pred |
	pred := visitedBlocks detect: [ :b | aDRBasicBlock hasPredecessor: b ].
	^ pred
]
