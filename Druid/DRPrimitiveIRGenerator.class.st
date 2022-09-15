Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
]

{ #category : #visiting }
DRPrimitiveIRGenerator >> finishCodeInterpretation: lastFrame [

	"Plug the IR with the two exit blocks"

	| conditionalJump |
	self spillStack.

	conditionalJump := self
		                   instantiateNoResultInstruction: DRBranchIfCondition
		                   operands: { 
				                   DREqualsThanComparison new.
				                   executionState primitiveFailureCode.
				                   0 asDRValue }.
	currentBasicBlock endInstruction: conditionalJump.
	controlFlowGraph primitiveExitBlock: currentBasicBlock.

	conditionalJump trueBranch: controlFlowGraph successExitBlock.
	controlFlowGraph successExitBlock addPredecessor: currentBasicBlock.

	conditionalJump falseBranch: controlFlowGraph failureExitBlock.
	controlFlowGraph failureExitBlock addPredecessor: currentBasicBlock.

	controlFlowGraph successExitBlock endInstruction: (self
			 instantiateNoResultInstruction: DRReturn
			 operands: { (DRConstantValue value: numberOfArguments + 1) }).

	"Make the end instruction of the failure exit finish with a Noop"
	controlFlowGraph failureExitBlock endInstruction:
		(self instantiateNoResultInstruction: DRPrimitiveFail)
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
	variableSpecialCases at: #primFailCode put: #interpretPrimFailCodeWith:.
	specialCases at: #cCoerceSimple:to: put: #interpretCCoerceWith:.	
]

{ #category : #inline }
DRPrimitiveIRGenerator >> inlineGenerator [

	^ DRPrimitiveIRGeneratorInline new
		  controlFlowGraph: controlFlowGraph;
		  yourself
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
				  DRInterpreterReference freeStartAddress }
]

{ #category : #'special cases' }
DRPrimitiveIRGenerator >> interpretPrimFailCodeAssignmentWith: aRBAssignmentNode [ 
	
	| newFailureCode copy |
	aRBAssignmentNode value acceptVisitor: self.
	newFailureCode := self popOperand.
	copy := self instructionFor: newFailureCode inNode: aRBAssignmentNode.
	self currentPrimitiveFailureCode: copy.
	self pushOperand: newFailureCode.
	^ newFailureCode
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> interpretPrimFailCodeWith: aRBNode [

	self pushOperand: executionState primitiveFailureCode
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
	
	super numberOfArguments: aValue.
	self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
]

{ #category : #'frame-access' }
DRPrimitiveIRGenerator >> popFrame [

	"If the interpretation stack is empty, we are returning from the primitive
	- generate a return instruction popping receiver and arguments all at once
	- amount to pop is expressed in bytes, so multiply by word size"

	| poppedFrame |
	self topFrame receiverWithState: executionState.
	poppedFrame := super popFrame.

	executionState executionStack size = 0 ifTrue: [ 
		self finishCodeInterpretation: poppedFrame ].

	^ poppedFrame
]

{ #category : #visiting }
DRPrimitiveIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments method: method [

	| instruction |
	instruction := self instantiate: DRMethod operands: { receiver } , arguments.
	instruction methodNode: method ast.
	self
		interpretCode: instruction
		receiver: receiver
		arguments: arguments
]

{ #category : #'pharo-stack' }
DRPrimitiveIRGenerator >> spillStack [

	"If there is no operand stack, the current block has no predecessors and is considered as dead code.
	Do not spill the stack here."
	self currentOperandStack ifNil: [ ^ self ].
	self currentOperandStack size > (numberOfArguments + 1)
		ifTrue: [ self error: 'Non Balanced Stack!' ].

	[ self currentOperandStack isEmpty ] whileFalse: [ | destinationRegister copyInstruction |
		destinationRegister := self currentOperandStack size = 1
			ifTrue: [ self receiverRegister ]
			ifFalse: [ DRPhysicalGeneralPurposeRegister name: 'Arg', (self currentOperandStack size - 2) asString, 'Reg' ].
		copyInstruction := DRCopy
			operands: { self currentOperandStack pop }
			result: destinationRegister.
		self currentBasicBlock addInstruction: copyInstruction.
	]
]

{ #category : #visiting }
DRPrimitiveIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	variableSpecialCases at: aRBVariableNode name ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBVariableNode } ].

	"Resolve it as a variableAccess"
	1halt.
	self pushOperand:
		((self topFrame homeFrameWithState: executionState) receiver value 
			 instVarNamed: aRBVariableNode name) asDRValue
]
