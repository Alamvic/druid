Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRMetaCompilerIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

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
]

{ #category : #inline }
DRPrimitiveIRGenerator >> inlineGenerator [

	^ DRPrimitiveIRGeneratorInline new
		  controlFlowGraph: controlFlowGraph;
		  executionState: executionState copy;
		  yourself
]

{ #category : #interpreting }
DRPrimitiveIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	| value |
	aRBAssignmentNode value acceptVisitor: self.
	self flag: #TODO. "Using a FrameReturn to keep the value for inlinings"
	value := currentBasicBlock frameReturn: self operandStackTop. 

	aRBAssignmentNode variable binding isInstanceVariable ifTrue: [
		^ self receiver value
			  instVarNamed: aRBAssignmentNode variable name
			  put: value ].

	aRBAssignmentNode variable binding originalVar isTempVariable
		ifTrue: [
			^ self topFrame
				  temporaryAt: aRBAssignmentNode variable name
				  put: value
				  withState: executionState ].

	self halt
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

{ #category : #'frame-access' }
DRPrimitiveIRGenerator >> popFrame [

	"If the interpretation stack is empty, we are returning from the primitive
	- generate a return instruction popping receiver and arguments all at once
	- amount to pop is expressed in bytes, so multiply by word size"

	| poppedFrame |
	self flag: #Remove.
	self topFrame receiverWithState: executionState.
	poppedFrame := super popFrame.
	^ poppedFrame
]

{ #category : #'frame-access' }
DRPrimitiveIRGenerator >> popFrameMergingDeferredReturns [

	| exitBasicBlock topReturns |
	exitBasicBlock := self topFrame exitBasicBlock.
	topReturns := self topFrame deferredMethodReturns.
	topReturns size > 0
		ifTrue: [
			| mergedState |
			self assert: exitBasicBlock predecessors isEmpty. "If not, check if new predecessors will be added"
			topReturns keysDo: [ :frameReturn | 
				frameReturn breakBasicBlock. "Breaking the block in case of be shared by many returns"
				frameReturn basicBlock jumpTo: exitBasicBlock ].
			mergedState := DRStackMerger new
				               builder: self;
				               mergeBlock: exitBasicBlock;
				               mergeAll: topReturns values upTo: self topFrame.
			executionState := mergedState.
			currentBasicBlock := exitBasicBlock ]
		ifFalse: [
			currentBasicBlock = exitBasicBlock ifFalse: [
				self assert: currentBasicBlock hasFinalInstruction.
				self assert: exitBasicBlock isEmptyBasicBlock ] ].

	"Returns installed in the CFG -> stop tracking"
	self topFrame clearDeferredMethodReturns.

	^ executionState popFrame
]

{ #category : #frames }
DRPrimitiveIRGenerator >> pushFrame: aDruidStackFrame [

	aDruidStackFrame returnValue: DRNullValue new.
	super pushFrame: aDruidStackFrame
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

	self pushOperand:
		((self topFrame homeFrameWithState: executionState) receiver simpleConstantFold value 
			 instVarNamed: aRBVariableNode name) asDRValue
]
