Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

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

{ #category : #accessing }
DRMethodIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self currentBasicBlock loadArgument: i) ]
]

{ #category : #'frame-access' }
DRMethodIRGenerator >> popFrameMergingDeferredReturns [

	self topFrame resolveDeferredMethodReturns: self.
	^ executionState popFrame
]

{ #category : #configure }
DRMethodIRGenerator >> receiver: aDRValue [

	self push: aDRValue
]

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	^ self
		  messageSendInstructionFor: aRBMessageNode
		  receiver: receiver
		  arguments: arguments
		  method: (self methodFor: aRBMessageNode withReceiver: receiver)
]

{ #category : #acc }
DRMethodIRGenerator >> visitGlobalVariableNode: aRBVariableNode [ 

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
