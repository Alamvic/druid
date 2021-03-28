Class {
	#name : #DRUnsignedCoercion,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRUnsignedCoercion >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnsignedCoercion: self
]
