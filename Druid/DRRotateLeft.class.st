Class {
	#name : #DRRotateLeft,
	#superclass : #DRArithmeticInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRRotateLeft >> acceptVisitor: aVisitor [

	^ aVisitor visitLeftRotate: self
]

{ #category : #types }
DRRotateLeft >> type [

	^ DRUnsignedIntegerType bits64
]
