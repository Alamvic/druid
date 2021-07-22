Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRIRGenerator,
	#instVars : [
		'success',
		'primitiveExitSuccessBlock',
		'primitiveExitFailureBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGenerator >> initialize [

	super initialize.
	primitiveExitSuccessBlock := self basicNewBasicBlock.
	primitiveExitFailureBlock := self basicNewBasicBlock.
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	specialCases at: #success: put: #interpretSuccessArgWith:.
	specialCases at: #primitiveFail put: #interpretPrimitiveFailWith:.
	specialCases at: #successful put: #interpretSuccessfulWith:.
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> interpretPrimitiveFailWith: aRBMessageNode [ 
	
	success := (DRPhiFunction joinedVariables: { success . (DRConstantValue value: false) })
			result: self allocateTemporaryRegister;
			origin: thisContext stack;
			yourself.
	self currentBasicBlock addInstruction: success.
	self pushOperand: DRNullValue new
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> interpretSuccessArgWith: aRBMessageNode [

	| argument |
	aRBMessageNode arguments first acceptVisitor: self.
	argument := self popOperand.
	success := (DRPhiFunction joinedVariables: { success . argument })
			result: self allocateTemporaryRegister;
			origin: thisContext stack;
			yourself.
	self currentBasicBlock addInstruction: success.
	self pushOperand: success
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> interpretSuccessfulWith: aRBMessageNode [ 

	self pushOperand: success
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> ir [

	^ DRControlFlowGraph new
		initialBasicBlock: firstBasicBlock;
		yourself
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> newBasicBlock [

	| newBasicBlock |
	newBasicBlock := self basicNewBasicBlock.
	currentBasicBlock := newBasicBlock.
	firstBasicBlock ifNil: [ | instruction |
		"If it is the first block, push the default success value"
		firstBasicBlock := currentBasicBlock.
		success := self
			instantiate: DRCopy
			operands: { DRConstantValue value: true }.
		newBasicBlock addInstruction: success ].
	^ currentBasicBlock

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
	super popFrame.
	interpretationStack size = 1 ifTrue: [
		| conditionalJump |
		self spillStack.
		
		(success isCopy and: [ success operand1 result value ])
			ifTrue: [
				"Generate a single jump, there was no failure.
				This could be simplified with a posterior SCCP pass"
				currentBasicBlock jumpTo: primitiveExitSuccessBlock ]
			ifFalse: [
				conditionalJump := self instantiate: DRConditionalJump operands: { success }.
				currentBasicBlock endInstruction: conditionalJump.
				conditionalJump trueBranch: primitiveExitSuccessBlock.
				primitiveExitSuccessBlock addPredecessor: currentBasicBlock.
				conditionalJump falseBranch: primitiveExitFailureBlock.
				primitiveExitFailureBlock addPredecessor: currentBasicBlock ].
		
		primitiveExitSuccessBlock endInstruction: (self
			instantiate: DRReturn
			operands: { DRConstantValue value: numberOfArguments + 1 "Number of things to pop" })..
		"Make the end instruction of the failure exit finish with a Noop"
		primitiveExitFailureBlock endInstruction: (self instantiate: DRNoop)
	]
]
