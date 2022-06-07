Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
]

{ #category : #'execution state' }
DRPrimitiveIRGenerator >> executionStack [
	
	^ executionState executionStack
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> ignoreMessageWith: aRBMessageNode [

	self pushOperand: (self noop: aRBMessageNode)
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> initialize [

	super initialize.
	controlFlowGraph successExitBlock:controlFlowGraph newBasicBlock.
	controlFlowGraph successExitBlock beExitBlock.
	controlFlowGraph failureExitBlock: controlFlowGraph newBasicBlock.
	controlFlowGraph failureExitBlock beExitBlock.
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	specialCases at: #primFailCode put: #interpretPrimFailCodeWith:.
	specialCases at: #cCoerceSimple:to: put: #interpretCCoerceWith:.	
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> interpretCCoerceWith: aRBMessageNode [

	^ aRBMessageNode arguments first acceptVisitor: self
]

{ #category : #'special cases' }
DRPrimitiveIRGenerator >> interpretFreeStartAssignmentWith: aRBAssignmentNode [

	| byteSize valueToStore |
	aRBAssignmentNode value acceptVisitor: self.
	valueToStore := self popOperand.

	byteSize := 8.
	^ self
		  addInstructionFrom: aRBAssignmentNode
		  instructionKind: DRStore
		  operands: { 
				  byteSize asDRValue.
				  valueToStore.
				  (DRInterpreterReference reference:
					   'objectMemory freeStartAddress') }
]

{ #category : #'special cases' }
DRPrimitiveIRGenerator >> interpretPrimFailCodeAssignmentWith: aRBAssignmentNode [ 
	
	| newFailureCode |
	aRBAssignmentNode value acceptVisitor: self.
	newFailureCode := self popOperand.
	self currentPrimitiveFailureCode: newFailureCode.
	self pushOperand: newFailureCode.
	^ newFailureCode
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> interpretPrimFailCodeWith: aRBNode [

	self pushOperand: executionState primitiveFailureCode
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> ir [

	^ controlFlowGraph
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> newBasicBlock [

	^ self newBasicBlockWithState: nil
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> newBasicBlockWithState: executionState [

	| newBasicBlock |
	newBasicBlock := super newBasicBlockWithState: executionState.
	firstBasicBlock ifNil: [ firstBasicBlock := newBasicBlock ].
	^ newBasicBlock
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> newCFG [
	
	^ DRPrimitiveControlFlowGraph new
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> numberOfArguments: aValue [ 
	
	numberOfArguments := aValue.
	self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
]

{ #category : #'frame-access' }
DRPrimitiveIRGenerator >> popFrame [

	"If the interpretation stack is empty, we are returning from the primitive
	- generate a return instruction popping receiver and arguments all at once
	- amount to pop is expressed in bytes, so multiply by word size"
	| comparison vm |
	vm := self topFrame receiverWithState: executionState.
	super popFrame.
	
	"Plug the IR with the two exit blocks"
	executionState executionStack size = 1 ifTrue: [
		| conditionalJump |
		self spillStack.

		currentBasicBlock addInstruction: (comparison := self
			instantiate: DREqualsThanComparison
			operands: { 
				executionState primitiveFailureCode.
				0 asDRValue }).
		conditionalJump := self
			instantiateNoResultInstruction: DRConditionalJump
			operands: { comparison }.
		currentBasicBlock endInstruction: conditionalJump.
		controlFlowGraph primitiveExitBlock: currentBasicBlock.
		
		conditionalJump trueBranch: controlFlowGraph successExitBlock.
		controlFlowGraph successExitBlock addPredecessor: currentBasicBlock.
		
		conditionalJump falseBranch: controlFlowGraph failureExitBlock.
		controlFlowGraph failureExitBlock addPredecessor: currentBasicBlock.
		
		controlFlowGraph successExitBlock endInstruction: (self
			instantiateNoResultInstruction: DRReturn
			operands: { DRConstantValue value: numberOfArguments + 1 "Number of things to pop" }).

		"Make the end instruction of the failure exit finish with a Noop"
		controlFlowGraph failureExitBlock endInstruction: (self instantiateNoResultInstruction: DRPrimitiveFail)
	]
	
]
