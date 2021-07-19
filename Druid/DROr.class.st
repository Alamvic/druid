Class {
	#name : #DROr,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROr >> acceptVisitor: aVisitor [

	^ aVisitor visitOr: self
]
