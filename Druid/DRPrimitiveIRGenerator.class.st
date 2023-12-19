"
Expects a primitive method, this is a method of an interpreter implementing a primitive (e.g. #primitiveAdd), and generates a Druid's IR.


"
Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRMetaCompilerIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'special cases' }
DRPrimitiveIRGenerator >> executeFullCogBlockClosureMayContextSwitchWith: aRBMessageNode [

	| closureCode mayContextSwitch offsetSelector |
	closureCode := self visitOperand: aRBMessageNode arguments first.
	mayContextSwitch := self visitOperand: aRBMessageNode arguments third.

	offsetSelector := DRInterpreterReference reference:
		                  (mayContextSwitch simpleConstantFold
			                   ifTrue: [ 'self fullBlockEntryOffset' ]
			                   ifFalse: [
			                   'self fullBlockNoContextSwitchEntryOffset' ]).

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRClosureActivation
		  operands: {
				  closureCode.
				  offsetSelector }
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
	specialCases at: #executeFullCogBlock:closure:mayContextSwitch: put: #executeFullCogBlockClosureMayContextSwitchWith:
]

{ #category : #visiting }
DRPrimitiveIRGenerator >> inlineGenerator [

	^ DRPrimitiveIRGeneratorInline new
		  controlFlowGraph: controlFlowGraph;
		  executionState: executionState copy;
		  typeSystem: self typeSystem;		
		  yourself
]

{ #category : #interpreting }
DRPrimitiveIRGenerator >> interpretAssignmentNode: aRBAssignmentNode [

	| value |
	aRBAssignmentNode value acceptVisitor: self.
	"Keep instruction in CFG for inlinings"
	value := currentBasicBlock strongCopy: self operandStackTop. 

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
	self
		  addInstructionFrom: aRBAssignmentNode
		  instructionKind: DRStore
		  operands: { 
				  byteSize asDRValue.
				  valueToStore.
				  DRInterpreterReference freeStartAddress }.
	
	"The result of executing a store is actually the stored value, not the store itself"
	self popOperand; pushOperand: valueToStore
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
			currentBasicBlock := exitBasicBlock ].

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

	| destinationRegister copyInstruction |
	"If there is no operand stack, the current block has no predecessors and is considered as dead code.
	Do not spill the stack here."
	self currentOperandStack ifNil: [ ^ self ].

	"Two cases can arrive here:
	  - either the primitive fails and did not touch the stack => no spill
	  - or the primitive succeeded and only one element is in the stack.
	We do not currently check this but the more relaxed version below.
	We do ignore all entries except the receiver entry in the stack however."

	self currentOperandStack size > (numberOfArguments + 1)
		ifTrue: [ self error: 'Non Balanced Stack!' ].

	"At the end of compilation, only the receiver register has an interesting value
	It woud be nice to check here at compile time that this is true and the other entries did not change."
	[ self currentOperandStack size > 1 ] whileTrue: [ self currentOperandStack pop ].
	
	destinationRegister := self receiverRegister.
	copyInstruction := DRCopy
		                   operands: { self currentOperandStack pop }
		                   result: destinationRegister.
	self currentBasicBlock addInstruction: copyInstruction
]
