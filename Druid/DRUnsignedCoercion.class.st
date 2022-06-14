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
	
	self haltIf: [ operand < 0 ].
	^ operand
]

{ #category : #types }
DRUnsignedCoercion >> type [
	
	^ DRIntegerType new
]
