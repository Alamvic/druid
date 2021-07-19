Class {
	#name : #DRPrimitiveIRGenerator,
	#superclass : #DRIRGenerator,
	#instVars : [
		'primitiveExitBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGenerator >> initialize [

	super initialize.
	primitiveExitBlock := self basicNewBasicBlock.	
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
	
	^ self currentBasicBlock jumpTo: self primitiveExitBlock
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> interpretSuccessArgWith: aRBMessageNode [

	| argument |
	aRBMessageNode arguments first acceptVisitor: self.
	argument := self popOperand.
	argument value = false
		ifFalse: [ self halt. "This is strange, somebody is setting it success to true explicitly" ].
	self pushOperand: DRNullValue new.
	
	^ self currentBasicBlock jumpTo: self primitiveExitBlock
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> interpretSuccessfulWith: aRBMessageNode [ 

	"If the primitive failed, we would have jumped already to the failure exit.
	So this is always true"
	^ self addInstructionFrom: aRBMessageNode instructionKind: DRConstantCondition operands: {DRConstantValue value: true}
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> ir [

	^ DRControlFlowGraph new
		initialBasicBlock: firstBasicBlock;
		yourself
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
	| result |
	firstBasicBlock := self currentFrame entryBasicBlock.
	result := super popFrame.
	interpretationStack size = 0 ifTrue: [
		self spillStack.
		self currentBasicBlock hasFinalInstruction ifFalse: [
			self currentBasicBlock endInstruction: (self
				instantiate: DRReturn
				operands: { DRConstantValue value: numberOfArguments + 1 "Number of things to pop" }).
		].
		"Make the end instruction of the failure exit finish with a Noop"
		primitiveExitBlock endInstruction: (self instantiate: DRNoop)
	].
		
	^ result
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> primitiveExitBlock [
	"Return a special block that represents the failure exit of the primitive"
	^ primitiveExitBlock
]
