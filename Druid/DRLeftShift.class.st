Class {
	#name : #DRLeftShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLeftShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLeftShift: self
]
