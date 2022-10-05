Class {
	#name : #DRBytecodeIRGenerator,
	#superclass : #DRMetaCompilerIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRBytecodeIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
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
	
	specialCases at: #fetchNextBytecode put: #interpretFetchNextBytecodeWith:.
	specialCases at: #push: put: #interpretPushWith:.
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretCCoerceWith: aRBMessageNode [

	^ aRBMessageNode arguments first acceptVisitor: self
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
	self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
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
