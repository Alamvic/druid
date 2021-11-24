Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	currentPrimitiveFailureCode := anInstruction
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
	specialCases at: #primFailCode put: #interpretPrimFailCodeWith:
]

{ #category : #'special cases' }
DRPrimitiveIRGenerator >> interpretPrimFailCodeAssignmentWith: aRBAssignmentNode [ 
	
	| copy |
	aRBAssignmentNode value acceptVisitor: self.
	
	"copy := instructionFactory copy: self popOperand."
	self currentBasicBlock primitiveFailureCode: self operandStackTop.
	"self pushOperand: copy"
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> interpretPrimFailCodeWith: aVariableNode [

	self pushOperand: self currentBasicBlock primitiveFailureCode
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> ir [

	^ controlFlowGraph
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> newBasicBlock [

	| newBasicBlock |
	newBasicBlock := super newBasicBlock.
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
	vm := self currentFrame receiver.
	super popFrame.
	
	"Plug the IR with the two exit blocks"
	interpretationStack size = 1 ifTrue: [
		| conditionalJump |
		self spillStack.

		currentBasicBlock addInstruction: (comparison := self
			instantiate: DREqualsThanComparison
			operands: { 
				currentBasicBlock primitiveFailureCode.
				0 asDRValue }).
		conditionalJump := self instantiate: DRConditionalJump operands: { comparison }.
		currentBasicBlock endInstruction: conditionalJump.
		controlFlowGraph primitiveExitBlock: currentBasicBlock.
		
		conditionalJump trueBranch: controlFlowGraph successExitBlock.
		controlFlowGraph successExitBlock addPredecessor: currentBasicBlock.
		
		conditionalJump falseBranch: controlFlowGraph failureExitBlock.
		controlFlowGraph failureExitBlock addPredecessor: currentBasicBlock.
		
		controlFlowGraph successExitBlock endInstruction: (self
			instantiate: DRReturn
			operands: { DRConstantValue value: numberOfArguments + 1 "Number of things to pop" }).

		"Make the end instruction of the failure exit finish with a Noop"
		controlFlowGraph failureExitBlock endInstruction: (self instantiate: DRPrimitiveFail)
	]
	
]
