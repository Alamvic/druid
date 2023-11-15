Class {
	#name : #DRUnsignedCoercion,
	#superclass : #DRUnaryInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRUnsignedCoercion >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnsignedCoercion: self
]

{ #category : #printing }
DRUnsignedCoercion >> opcode [
	
	^ '(unsigned)'
]

{ #category : #printing }
DRUnsignedCoercion >> sccpOperateOnLattice: operand [

	self haltIf: [ operand isNumber and: [ operand < 0 ] ].
	^ operand
]
