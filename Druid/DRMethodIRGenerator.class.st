Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodIRGenerator >> executionState: aDRExecutionState [

	executionState := aDRExecutionState
]

{ #category : #visiting }
DRMethodIRGenerator >> finishCodeInterpretation: lastFrame [

	| returnValue |
	returnValue := lastFrame mirrorFrame returnValue ifNil: [ lastFrame returnValue ].

	currentBasicBlock return: returnValue
]

{ #category : #inline }
DRMethodIRGenerator >> inlineGenerator [

	| newIR |
	newIR := DRMethodIRGeneratorInline new
		         controlFlowGraph: controlFlowGraph;
		         yourself.
	newIR executionState: executionState copy.
	^ newIR
]

{ #category : #interpreting }
DRMethodIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	| value |
	value := self visitOperand: aRBAssignmentNode value.

	aRBAssignmentNode variable binding isInstanceVariable ifTrue: [ 
		^ self
			  addInstructionFrom: aRBAssignmentNode
			  instructionKind: DRStoreInstanceVariable
			  operands: { 
					  self receiver.
					  aRBAssignmentNode variable binding index asDRValue.
					  value } ].

	aRBAssignmentNode variable binding originalVar isTempVariable ifTrue: [ 
		^ self
			  addInstructionFrom: aRBAssignmentNode
			  instructionKind: DRStoreTemporaryVariable
			  operands: { 
					  aRBAssignmentNode variable name asDRValue. "Fix temporary names collisions"
					  value } ].

	self unexplored
]

{ #category : #'deferred returns' }
DRMethodIRGenerator >> mergeDeferredReturns: deferredMethodReturns in: aDRBasicBlock [

	| frameReturns stackFrame |
	stackFrame := deferredMethodReturns values first topFrame.
	currentBasicBlock := aDRBasicBlock.
	frameReturns := deferredMethodReturns keys.
	frameReturns do: [ :frameReturn | 
		(aDRBasicBlock hasPredecessor: frameReturn basicBlock) ifTrue: [ "Remove to keep the order" 
			aDRBasicBlock removePredecessor: frameReturn basicBlock ].
		frameReturn basicBlock jumpTo: aDRBasicBlock ].
	self addInstructionFrom: stackFrame method instructionKind: DRPhiFunction operands: frameReturns.
	stackFrame returnValue: self popOperand.
	^ self addFrameReturn: stackFrame
]

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	^ self
		  messageSendInstructionFor: aRBMessageNode
		  receiver: receiver
		  arguments: arguments
		  method: (self methodFor: aRBMessageNode withReceiver: receiver)
]

{ #category : #visiting }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstanceVariable
		operands: { 
				self topFrame receiver.
				aRBVariableNode name asDRValue }
]

{ #category : #visiting }
DRMethodIRGenerator >> visitTemporaryVariableNode: aRBVariableNode [

	| value |
	value := self temporaryAt: aRBVariableNode name.

	value isNullValue ifTrue: [ "If Null then it is a method local variable"
		^ self
			  addInstructionFrom: aRBVariableNode
			  instructionKind: DRLoadTemporaryVariable
			  operands: { aRBVariableNode name asDRValue } ].

	"Else, it is an argument"
	^ self pushOperand: value
]
