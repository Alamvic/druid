Class {
	#name : #DRUnsignedRightShift,
	#superclass : #DRSignedRightShift,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRUnsignedRightShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitUnsignedRightShift: self
]

{ #category : #types }
DRUnsignedRightShift >> type [

	^ DRUnsignedIntegerType size: 8 "bytes per word"
]
