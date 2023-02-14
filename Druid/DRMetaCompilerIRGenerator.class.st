Class {
	#name : #DRMetaCompilerIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #factory }
DRMetaCompilerIRGenerator >> createInitialBasicBlock [
	| initialBasicBlock |
	initialBasicBlock := super createInitialBasicBlock.
	self currentPrimitiveFailureCode: (initialBasicBlock copy: 0).
	^ initialBasicBlock
]

{ #category : #accessing }
DRMetaCompilerIRGenerator >> currentPrimitiveFailureCode: anInstruction [
	
	self executionState primitiveFailureCode: anInstruction
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> finishCodeInterpretation: lastFrame [

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

{ #category : #initialization }
DRMetaCompilerIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	
	variableSpecialCases at: #instructionPointer put: #interpretInstructionPointerWith:.
	
	"Stack access"
	specialCases at: #stackTop put: #interpretStackTopWith:.
	specialCases at: #stackValue: put: #interpretStackValueWith:.
	specialCases at: #pop:thenPush: put: #interpretPopThenPushWith:.
	specialCases at: #pop: put: #interpretPopThenPushWith:.
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> methodFor: aRBMessageNode withReceiver: receiver [

	| receiverType methodFound |
	receiverType := aRBMessageNode isSuperSend
		               ifTrue: [ self currentMethod methodClass superclass ]
		               ifFalse: [ receiver type ].
	methodFound := receiverType lookupSelector: aRBMessageNode selector.
	methodFound ifNil: [
		self error:
			'lookup failed for: ' , aRBMessageNode selector , ' in class '
			, receiverType name ].
	^ methodFound
]

{ #category : #accessing }
DRMetaCompilerIRGenerator >> numberOfArguments: aValue [ 
	
	super numberOfArguments: aValue.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
]

{ #category : #configure }
DRMetaCompilerIRGenerator >> receiver: aDRValue [

	self push: self receiverRegister
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	| instruction method |
	method := self methodFor: aRBMessageNode withReceiver: receiver.
	
	"If the method has a special ir builder case, use that"
	method ast pragmaNamed: #druidIRBuilder: ifPresent: [ :pragma |
		^ self perform: pragma arguments first value withArguments: { aRBMessageNode } ].
	
	"Otherwise, interpret the method recursively"
	instruction := self
		               instantiate: DRMethod
		               operands: { receiver } , arguments.
	instruction methodNode: method ast.
	self
		interpretCode: instruction
		receiver: receiver
		arguments: arguments
]

{ #category : #visiting }
DRMetaCompilerIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	variableSpecialCases at: aRBVariableNode name ifPresent: [ :mapping | 
		^ self perform: mapping withArguments: { aRBVariableNode } ].

	"Resolve it as a variableAccess"
	self pushOperand:
		((self topFrame homeFrameWithState: executionState) receiver simpleConstantFold value 
			 instVarNamed: aRBVariableNode name) asDRValue
]
