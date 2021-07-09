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
	pharoOperandStack := Stack new.
	primitiveExitBlock := self basicNewBasicBlock.	
]

{ #category : #'special-cases' }
DRPrimitiveIRGenerator >> initializeSpecialCases [

	super initializeSpecialCases.
	specialCases at: #success: put: #interpretSuccessArgWith:.
	specialCases at: #primitiveFail put: #interpretPrimitiveFailWith:.
	specialCases at: #successful put: #interpretSuccessfulWith:.
]

{ #category : #API }
DRPrimitiveIRGenerator >> interpretMethod: aMethodAST receiver: aReceiver arguments: arguments [

	| result |
	result := super interpretMethod: aMethodAST receiver: aReceiver arguments: arguments.

	"If the interpretation stack is empty, we are returning from the primitive
	- generate a return instruction popping receiver and arguments all at once
	- amount to pop is expressed in bytes, so multiply by word size"
	interpretationStack ifNotEmpty: [ ^ result ].
	self spillOperandStack.
	^ self currentBasicBlock endInstruction: (self
		instantiate: DRReturn
		operands: { DRConstantValue value: numberOfArguments + 1 "Number of things to pop" }).
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
		exitBasicBlock: primitiveExitBlock;
		yourself
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> numberOfArguments: aValue [ 
	
	numberOfArguments := aValue.
	self push: self receiverRegister.
	0 to: numberOfArguments - 1 do: [ :i |
		self push: (self argRegisterNumber: i) ]
]

{ #category : #asd }
DRPrimitiveIRGenerator >> pop [

	pharoOperandStack ifEmpty: [ self error: 'Operand stack underflow!' ].
	^ pharoOperandStack pop
]

{ #category : #'operand-stack' }
DRPrimitiveIRGenerator >> pop: anInteger [ 

	anInteger timesRepeat: [ self pop ]
]

{ #category : #asd }
DRPrimitiveIRGenerator >> popOperand [

	^ self currentFrame pop
]

{ #category : #accessing }
DRPrimitiveIRGenerator >> primitiveExitBlock [
	"Return a special block that represents the failure exit of the primitive"
	^ primitiveExitBlock
]

{ #category : #'operand-stack' }
DRPrimitiveIRGenerator >> push: aValue [
	
	"Update the operand stack to keep track of the new position"
	pharoOperandStack push: aValue.
	"Each position of the operand stack is related to a special register.
	Translate the push to a copy with a concrete result register.
	The concrete result register will be the one associated with the current position of the operand stack"
	"self haltIf: [ aValue isKindOf: DRNullValue ].
	copyInstruction := self instantiate: DRCopy operands: { aValue }.
	destinationRegister := operandStackPosition = 1
		ifTrue: [ self receiverRegister ]
		ifFalse: [ DRPhysicalRegister name: 'Arg', (operandStackPosition - 2) asString, 'Reg' ].
	
	copyInstruction result: destinationRegister.
	^ self currentBasicBlock addInstruction: copyInstruction"
]

{ #category : #asd }
DRPrimitiveIRGenerator >> pushOperand: anOperand [

	self currentFrame push: anOperand
]

{ #category : #'operand-stack' }
DRPrimitiveIRGenerator >> spillOperandStack [
	
	pharoOperandStack size > (numberOfArguments + 1)
		ifTrue: [ self error: 'Non Balanced Stack!' ].

	[ pharoOperandStack isEmpty ] whileFalse: [ | destinationRegister copyInstruction |
		destinationRegister := pharoOperandStack size = 1
			ifTrue: [ self receiverRegister ]
			ifFalse: [ DRPhysicalRegister name: 'Arg', (pharoOperandStack size - 2) asString, 'Reg' ].
		copyInstruction := DRCopy
			operands: { pharoOperandStack pop }
			result: destinationRegister.
		self currentBasicBlock addInstruction: copyInstruction.
	]
]

{ #category : #'operand-stack' }
DRPrimitiveIRGenerator >> stackTop [
	
	^ pharoOperandStack top
]
