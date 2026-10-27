Extension { #name : 'DRIRGenerator' }

{ #category : '*Druid-Cogit' }
DRIRGenerator >> addInstructionCall: operands from: aRBMessageNode [

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRCall
		  operands: operands
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> argRegisterNumber: anInteger [

	"TODO: Merge with #interpretStackValueWith: aRBMessageNode withValue: aValue ?"

	| loadArgument |
	numberOfArguments = anInteger ifTrue: [
		^ self instructionFactory loadBytecodeReceiver classType:
			  (DRSignedIntegerType size: 8) ].

	loadArgument := (self instructionFactory loadArgument: anInteger)
		                classType: (DRSignedIntegerType size: 8).
	loadArgument origin: thisContext sender stack.
	self currentBasicBlock addInstruction: loadArgument.
	^ loadArgument
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretAsFloatWith: aRBMessageNode [

	| receiver |
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.

	"If it's already a float, do nothing"
	receiver type isFloatType ifTrue: [ ^ self pushOperand: receiver ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRIntToFloat
		  operands: { receiver }
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretAsIntegerWith: aRBMessageNode [

	| receiver |
	aRBMessageNode receiver acceptVisitor: self.
	receiver := self popOperand.

	"If it's already an integer, do nothing"
	receiver type isFloatType ifTrue: [ 
		^ self
			  addInstructionFrom: aRBMessageNode
			  instructionKind: DRFloatToInt
			  operands: { receiver } ].
	receiver type isBooleanType ifTrue: [ 
		^ self pushOperand: receiver].

	"Otherwise"
	^ self pushOperand: receiver
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretAsUnsignedIntegerWith: aRBMessageNode [ 
	
	^ self interpretUnaryInstruction: aRBMessageNode instructionKind: DRUnsignedCoercion
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretCCoerceWith: aRBMessageNode [

	| typeName inner |
	aRBMessageNode arguments first acceptVisitor: self.
	inner := self popOperand.

	typeName := aRBMessageNode arguments second value.
	(typeName = 'sqInt' and: [ inner type isUnsigned ]) ifTrue: [
		^ self
			  addInstructionFrom: aRBMessageNode arguments first
			  instructionKind: DRSignedCoercion
			  operands: { inner } ].

	^ self pushOperand: inner
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretFloat64FromRawBitsWith: aRBMessageNode [

	| receiver |
	aRBMessageNode arguments first acceptVisitor: self.
	receiver := self popOperand.

	receiver type isIntegerType ifFalse: [
		^ self error: 'cannot extract float bits from a non float value' ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRGetFloatFromBits
		  operands: { receiver }
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> interpretRawFloatBitsWith: aRBMessageNode [ 

	| receiver |
	aRBMessageNode arguments first acceptVisitor: self.
	receiver := self popOperand.

	receiver type isFloatType
		ifFalse: [ ^ self error: 'cannot extract float bits from a non float value' ].

	^ self
		  addInstructionFrom: aRBMessageNode
		  instructionKind: DRGetFloatBits
		  operands: { receiver }
]

{ #category : '*Druid-Cogit' }
DRIRGenerator >> receiverRegister [

	^ DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg'
]
