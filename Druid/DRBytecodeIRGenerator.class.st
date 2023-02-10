Class {
	#name : #DRBytecodeIRGenerator,
	#superclass : #DRPrimitiveIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRBytecodeIRGenerator >> finishCodeInterpretation: lastFrame [

	self spillStack
]

{ #category : #accessing }
DRBytecodeIRGenerator >> initialize [

	super initialize.
	controlFlowGraph successExitBlock:controlFlowGraph newBasicBlock.
	controlFlowGraph successExitBlock beExitBlock.
	controlFlowGraph failureExitBlock: controlFlowGraph newBasicBlock.
	controlFlowGraph failureExitBlock beExitBlock.
]

{ #category : #accessing }
DRBytecodeIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	variableSpecialCases at: #framePointer put: #interpretFramePointerWith:.
	"variableSpecialCases at: #currentBytecode put: #interpretCurrentBytecodeWith:."
	
	specialCases at: #internalMustBeBoolean put: #interpretMustBeBooleanWith:.
	specialCases at: #fetchNextBytecode put: #interpretFetchNextBytecodeWith:.
	specialCases at: #push: put: #interpretPushWith:.
	specialCases at: #normalLiteralSelectorAt:argumentCount: put: #interpretNormalLiteralSendWith:.
	specialCases at: #normalSendSpecialSelector:argumentCount: put: #interpretNormalSpecialSelectorSendWith:.
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretArgumentCountAssignmentWith: aRBAssignmentNode [ 
	
	"Ok, we are probably doing a message send!
	Visit the value of the assignment and get the number of arguments.
	For now do not use it"
	aRBAssignmentNode value acceptVisitor: self.
]

{ #category : #visiting }
DRBytecodeIRGenerator >> interpretCurrentBytecodeWith: aRBVariableNode [ 
	
	^ self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRCopy
		operands: { DRJitCompileTimeExpression new expression: 'byte0' }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretFetchNextBytecodeWith: aRBMessageNode [ 
	
	"Push a 1 for now"
	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRCopy
		operands: { 1 asDRValue }
]

{ #category : #visiting }
DRBytecodeIRGenerator >> interpretFramePointerWith: aRBVariableNode [ 
	
	^ self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadFramePointer
		operands: #()
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretInstructionPointerAssignmentWith: aRBAssignmentNode [ 

	"Changing the bytecode instruction pointer, we must generate a jump to the new target"
	
	| newBytecodePC |
	aRBAssignmentNode value acceptVisitor: self.
	newBytecodePC := self popOperand.
	
	^ self
		  addInstructionWithNoResultFrom: aRBAssignmentNode
		  instructionKind: DRBytecodeJump
		  operands: { newBytecodePC asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretInstructionPointerWith: aRBVariableNode [
	
	^ self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRCopy
		operands: { DRJitCompileTimeExpression new expression: 'bytecodePC' }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretMustBeBooleanWith: aRBMessageNode [ 
	
	"self CallRT: ceSendMustBeBooleanTrampoline"
	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRCall
		  operands: { 'ceSendMustBeBooleanTrampoline' }
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretNormalLiteralSendWith: aMessage [

	"Interpret a send. In the interpreter, this is a send where we don't know yet the receiver or its class.
	We know the numberOfArguments that we should use to extract the receiver, then its class.
	
	Translate this as a send instruction"
	| selectorIndex argumentCount |
	aMessage arguments first acceptVisitor: self.
	selectorIndex := self popOperand.
	
	aMessage arguments second acceptVisitor: self.
	argumentCount := self popOperand.
	
	^ self
		  addInstructionWithNoResultFrom: aMessage
		  instructionKind: DRJITMessageSend
		  operands: { selectorIndex asDRValue. argumentCount asDRValue . false asDRValue }
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretNormalSpecialSelectorSendWith: aMessage [

	"Interpret a send. In the interpreter, this is a send where we don't know yet the receiver or its class.
	We know the numberOfArguments that we should use to extract the receiver, then its class.
	
	Translate this as a send instruction"
	| selectorIndex argumentCount |
	aMessage arguments first acceptVisitor: self.
	selectorIndex := self popOperand.
	
	aMessage arguments second acceptVisitor: self.
	argumentCount := self popOperand.
	
	^ self
		  addInstructionWithNoResultFrom: aMessage
		  instructionKind: DRJITMessageSend
		  operands: { selectorIndex asDRValue. argumentCount asDRValue . true asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretPrimFailCodeAssignmentWith: aRBAssignmentNode [ 
	
	| newFailureCode copy |
	aRBAssignmentNode value acceptVisitor: self.
	newFailureCode := self popOperand.
	copy := self instructionFor: newFailureCode inNode: aRBAssignmentNode.
	self currentPrimitiveFailureCode: copy.
	self pushOperand: newFailureCode.
	^ newFailureCode
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretPrimFailCodeWith: aRBNode [

	self pushOperand: executionState primitiveFailureCode
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretPushWith: aRBMessageNode [ 
	
	| receiver |
	receiver := self visitOperand: aRBMessageNode arguments first.
	^ self addInstructionWithNoResultFrom: aRBMessageNode instructionKind: DRPush operands: { receiver }
]

{ #category : #'special cases - stack' }
DRBytecodeIRGenerator >> interpretStackTopWith: aRBMessageNode [

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRLoadStackValue
		  operands: { 0 asDRValue }
]

{ #category : #'special cases - stack' }
DRBytecodeIRGenerator >> interpretStackValueWith: aRBMessageNode [

	"Interpret messages of the form 
	interpreter stackValue: xxx.
	"

	| value |
	aRBMessageNode arguments first acceptVisitor: self.
	value := self popOperand.
	value operand1 value isInteger ifFalse: [ 
		self error: 'expected an integer value' ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRLoadStackValue
		  operands: { value }
]

{ #category : #accessing }
DRBytecodeIRGenerator >> newBasicBlock [

	^ self newBasicBlockWithState: nil
]

{ #category : #accessing }
DRBytecodeIRGenerator >> newBasicBlockWithState: executionState [

	| newBasicBlock |
	newBasicBlock := super newBasicBlockWithState: executionState.
	firstBasicBlock ifNil: [ firstBasicBlock := newBasicBlock ].
	^ newBasicBlock
]

{ #category : #accessing }
DRBytecodeIRGenerator >> newCFG [
	
	^ DRPrimitiveControlFlowGraph new
]

{ #category : #accessing }
DRBytecodeIRGenerator >> numberOfArguments: aValue [ 
	
	numberOfArguments := aValue.
	"self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]"
]

{ #category : #accessing }
DRBytecodeIRGenerator >> pop [

	self
		addInstructionFrom: nil
		instructionKind: DRPop
		operands: #().
	^ self popOperand
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> push: aValue [

	self
		addInstructionWithNoResultFrom: nil
		instructionKind: DRPush
		operands: { aValue }.
	^ self popOperand
]

{ #category : #accessing }
DRBytecodeIRGenerator >> receiver: aDRValue [
]

{ #category : #visiting }
DRBytecodeIRGenerator >> visitClassVariableNode: aRBVariableNode [

	| value mappings |
	value := aRBVariableNode variable name.
	mappings := { 'FoxIFReceiver' -> 'FrameReceiverOffset' } asDictionary.
	value := mappings at: value ifAbsent: [ value ].
	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { (DRJitCompileTimeExpression new expression: value) }
]
