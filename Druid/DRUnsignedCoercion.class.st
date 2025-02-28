Class {
	#name : 'DRUnsignedCoercion',
	#superclass : 'DRUnaryInstruction',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRUnsignedCoercion >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnsignedCoercion: self
]

{ #category : 'printing' }
DRUnsignedCoercion >> opcode [
	
	^ '(unsigned)'
]

{ #category : 'printing' }
DRUnsignedCoercion >> sccpOperateOnLattice: operand [

	self assert: operand isNumber.

	operand < 0 ifTrue: [
		operand twoComplementOfBitSize: self type numberOfBits ].
	^ operand
]
