Class {
	#name : #DRBytecodeIRGenerator,
	#superclass : #DRPrimitiveIRGenerator,
	#instVars : [
		'jumpOffset'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #adding }
DRBytecodeIRGenerator >> addAnnotateBytecode: aRBMethodNode [

	self
		  addInstructionWithNoResultFrom: aRBMethodNode
		  instructionKind: DRAnnotateBytecode
		  operands: #(  ).
	^ self popOperand "Let the stack empty"
]

{ #category : #visiting }
DRBytecodeIRGenerator >> finishCodeInterpretation: lastFrame [

	"Nothing"
]

{ #category : #accessing }
DRBytecodeIRGenerator >> initialize [

	super initialize.
	jumpOffset := 0.

	controlFlowGraph successExitBlock:controlFlowGraph newBasicBlock.
	controlFlowGraph successExitBlock beExitBlock.
	controlFlowGraph failureExitBlock: controlFlowGraph newBasicBlock.
	controlFlowGraph failureExitBlock beExitBlock.
]

{ #category : #accessing }
DRBytecodeIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	variableSpecialCases at: #framePointer put: #interpretFramePointerWith:.
	variableSpecialCases at: #stackPointer put: #interpretStackPointerWith:.
	variableSpecialCases at: #stackLimit put: #interpretStackLimitWith:.
	variableSpecialCases at: #extA put: #interpretAsCompilerVariableWith:.
	variableSpecialCases at: #extB put: #interpretAsCompilerVariableWith:.
	variableSpecialCases at: #numExtA put: #interpretAsCompilerVariableWith:.
	variableSpecialCases at: #numExtB put: #interpretAsCompilerVariableWith:.
	"variableSpecialCases at: #currentBytecode put: #interpretCurrentBytecodeWith:."

	specialCases at: #iframeNumArgs: put: #interpretNumArgsWith:.
	specialCases at: #iframeMethod: put: #interpretIframeMethodWith:.
	specialCases at: #itemporary:in: put: #interpretITemporaryInWith:.
	specialCases at: #itemporary:in:put: put: #interpretITemporaryInPutWith:.
	specialCases at: #internalMustBeBoolean: put: #interpretMustBeBooleanWith:.
	specialCases at: #fetchNextBytecode put: #interpretFetchNextBytecodeWith:.
	specialCases at: #commonReturn: put: #interpretCommonReturnWith:.
	specialCases at: #commonCallerReturn: put: #interpretCommonCallerReturnWith:.
	specialCases at: #checkForEventsMayContextSwitch: put: #interpretInterruptCheckWith:.
	specialCases at: #fetchByte put: #interpretFetchByteWith:.
	specialCases at: #druidForceIntepretation put: #interpretDruidForceIntepretationWith:.
	
	specialCases at: #createFullClosureInIndex:numCopied:ignoreContext: put: #interpretCreateClosureWith:.

	"Message send"
	specialCases at: #normalLiteralSelectorAt:argumentCount: put: #interpretNormalLiteralSendWith:.
	specialCases at: #normalSendSpecialSelector:argumentCount: put: #interpretNormalSpecialSelectorSendWith:.
	specialCases at: #sendSuper:numArgs: put: #interpretSuperSendWith:.
	specialCases at: #sendDirectSuper:numArgs: put: #interpretDirectSuperSendWith:.
	

]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretArgumentCountAssignmentWith: aRBAssignmentNode [ 
	
	"Ok, we are probably doing a message send!
	Visit the value of the assignment and get the number of arguments.
	For now do not use it"
	aRBAssignmentNode value acceptVisitor: self.
]

{ #category : #'special-cases' }
DRBytecodeIRGenerator >> interpretAsCompilerVariableAssignmentWith: aRBAssignmentNode [

	| operand |
	operand := self visitOperand: aRBAssignmentNode value.
	(self
		 addInstructionWithNoResultFrom: aRBAssignmentNode
		 instructionKind: DRCopy
		 operands: { operand }) result:
		(DRStagedRegister name: aRBAssignmentNode variable name)
]

{ #category : #'special-cases' }
DRBytecodeIRGenerator >> interpretAsCompilerVariableWith: aRBVariableNode [

	^ self
		  addCopy: aRBVariableNode
		  with: ((DRInterpreterReference reference: aRBVariableNode name)
				   type: (DRSignedIntegerType size: 8);
				   yourself)
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretCommonCallerReturnWith: aRBMessageNode [

	| returnedValue |
	returnedValue := self visitOperand: aRBMessageNode arguments first.

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRReturn
		  operands: { returnedValue . true asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretCommonReturnWith: aRBMessageNode [

	| returnedValue |
	returnedValue := self visitOperand: aRBMessageNode arguments first.

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRReturn
		  operands: { returnedValue . false asDRValue }
]

{ #category : #'as yet unclassified' }
DRBytecodeIRGenerator >> interpretCreateClosureWith: aRBMessageNode [

	| operands |
	operands := aRBMessageNode arguments collect: [ :e |
		            self visitOperand: e ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRClosureCreation
		  operands: operands
]

{ #category : #visiting }
DRBytecodeIRGenerator >> interpretCurrentBytecodeWith: aRBVariableNode [

	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { ((DRInterpreterReference reference: 'byte0')
				   type: (DRUnsignedIntegerType size: 1);
				   yourself) }
]

{ #category : #'special-cases' }
DRBytecodeIRGenerator >> interpretDirectSuperSendWith: aRBMessageNode [

	^ self
		  interpretSendWith: aRBMessageNode
		  specialSelector: false
		  sendTable: '(directedSendUsesBinding
								ifTrue: [directedSuperBindingSendTrampolines]
								ifFalse: [directedSuperSendTrampolines])' "TODO: improve"
]

{ #category : #'as yet unclassified' }
DRBytecodeIRGenerator >> interpretDruidForceIntepretationWith: aRBMessageNode [

	self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRDeoptimize
		operands: #(  ).

	"Cut control flow"
	self popOperand.
	^ (RBReturnNode value: (RBLiteralNode value: 0)) acceptVisitor: self
]

{ #category : #'special-cases' }
DRBytecodeIRGenerator >> interpretFetchByteWith: aRBMessageNode [

	jumpOffset := jumpOffset + 1.
	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRCopy
		  operands:
			  { ((DRInterpreterReference reference:
					    'byte' , jumpOffset asString)
				   type: (DRUnsignedIntegerType size: 1);
				   yourself) }
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
DRBytecodeIRGenerator >> interpretITemporaryInPutWith: aRBMessageNode [ 

	| index value |
	"Skip second argument, it's the frame pointer"
	index := (self visitOperand: aRBMessageNode arguments first) simpleConstantFold.	
	value := (self visitOperand: aRBMessageNode arguments third) simpleConstantFold.

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRStoreTemporaryVariable
		  operands: { index asDRValue . value asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretITemporaryInWith: aRBMessageNode [

	| index |
	index := (self visitOperand: aRBMessageNode arguments first) simpleConstantFold.

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRLoadTemporaryVariable
		  operands: { index asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretIframeMethodWith: aRBMessageNode [

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRCopy
		  operands:
		  { (DRInterpreterReference objectReference: 'methodObj') }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretInstructionPointerAssignmentWith: aRBAssignmentNode [ 

	"Changing the bytecode instruction pointer, we must generate a jump to the new target"
	
	aRBAssignmentNode value acceptVisitor: self.	
	^ self
		  addInstructionWithNoResultFrom: aRBAssignmentNode
		  instructionKind: DRBytecodeJump
		  operands: { self popOperand }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretInstructionPointerWith: aRBVariableNode [

	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRAdd
		  operands: {
				  ((DRInterpreterReference reference: 'bytecodePC')
					   type: (DRUnsignedIntegerType size: 1);
					   yourself).
				  jumpOffset asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretInterruptCheckWith: aRBMessageNode [

	^ (self
		   addInstructionCall: { 'ceCheckForInterruptTrampoline' }
		   from: aRBMessageNode) beMapped
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretMustBeBooleanWith: aRBMessageNode [

	| theNonBoolean |
	theNonBoolean := (self visitOperand: aRBMessageNode arguments first) simpleConstantFold.
	^ (self
		   addInstructionCall: {
				   'ceSendMustBeBooleanTrampoline'.
				   theNonBoolean }
		   from: aRBMessageNode) beMapped
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretNormalLiteralSendWith: aMessage [

	^ self
		  interpretSendWith: aMessage
		  specialSelector: false
		  sendTable: 'ordinarySendTrampolines'
]

{ #category : #accessing }
DRBytecodeIRGenerator >> interpretNormalSpecialSelectorSendWith: aMessage [

	^ self
		  interpretSendWith: aMessage
		  specialSelector: true
		  sendTable: 'ordinarySendTrampolines'
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretNumArgsWith: aRBMessageNode [ 

	^ self
		addInstructionFrom: aRBMessageNode
		instructionKind: DRCopy
		operands: { DRInterpreterReference reference: 'methodOrBlockNumArgs' }
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

{ #category : #'special-cases' }
DRBytecodeIRGenerator >> interpretSendWith: aRBMessageNode specialSelector: isSpecialSelector sendTable: sendTableName [
	"Interpret a send. In the interpreter, this is a send where we don't know yet the receiver or its class.
	We know the numberOfArguments that we should use to extract the receiver, then its class.
	
	Translate this as a send instruction"

	| selectorIndex argumentCount |
	selectorIndex := self visitOperand: aRBMessageNode arguments first.
	argumentCount := self visitOperand: aRBMessageNode arguments second.

	"In cogit, the special selectors at index X should be compiled as -(X+1)"
	isSpecialSelector ifTrue: [
		self
			addInstructionFrom: aRBMessageNode
			instructionKind: DRAdd
			operands: {
					selectorIndex.
					1 asDRValue }.
		self
			addInstructionFrom: aRBMessageNode
			instructionKind: DRNegate
			operands: { self popOperand }.
		selectorIndex := self popOperand ].

	"DRJITMessageSend will be translated using a instrinsic that already annotate the bytecode"
	self assert: controlFlowGraph hasAnnotatedBytecode not.

	^ self
		  addInstructionWithNoResultFrom: aRBMessageNode
		  instructionKind: DRJITMessageSend
		  operands: {
				  selectorIndex asDRValue.
				  argumentCount asDRValue.
				  sendTableName asDRValue }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretStackLimitWith: aRBVariableNode [

	| reference load |
	reference := DRInterpreterReference reference:
		             'coInterpreter stackLimitAddress'.
	load := self
		        addInstructionFrom: aRBVariableNode
		        instructionKind: DRLoad
		        operands: {
				        (self typeOf: aRBVariableNode).
				        reference asMemoryAddress }.
	^ load
]

{ #category : #visiting }
DRBytecodeIRGenerator >> interpretStackPointerWith: aRBVariableNode [ 
	
	^ self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadStackPointer
		operands: #()
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

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRLoadStackValue
		  operands: { self popOperand }
]

{ #category : #'special cases' }
DRBytecodeIRGenerator >> interpretSuperSendWith: aRBMessageNode [

	^ self
		  interpretSendWith: aRBMessageNode
		  specialSelector: false
		  sendTable: 'superSendTrampolines'
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

{ #category : #'stack management' }
DRBytecodeIRGenerator >> pushFrameForCode: aDRCode receiver: aReceiver [

	| frame |
	frame := super pushFrameForCode: aDRCode receiver: aReceiver.

	"If this is the base frame, it should continue with the next bytecode at the end"
	executionState baseFrame = frame ifTrue: [
		frame exitBasicBlock endInstruction:
			(self instantiateNoResultInstruction: DRContinueNextBytecode) ].

	^ frame
]

{ #category : #accessing }
DRBytecodeIRGenerator >> receiver: aDRValue [
]

{ #category : #visiting }
DRBytecodeIRGenerator >> visitClassVariableNode: aRBVariableNode [

	| name value mappings |
	name := aRBVariableNode variable name.
	mappings := {
		            ('FoxIFReceiver'
		             ->
			             ((DRInterpreterReference reference:
					               'FrameReceiverOffset')
				              type: (DRUnsignedIntegerType size: 8);
				              yourself)).
		            ('IMMUTABILITY' -> true asDRValue) } asDictionary.
	value := mappings at: name ifAbsent: [
		         (DRInterpreterReference reference: name)
			         type: (DRUnsignedIntegerType size: 8);
			         yourself ].
	^ self
		  addInstructionFrom: aRBVariableNode
		  instructionKind: DRCopy
		  operands: { value }
]
