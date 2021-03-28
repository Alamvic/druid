Class {
	#name : #DRRightShift,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRRightShift >> acceptVisitor: aVisitor [
	
	^ aVisitor visitRightShift: self
]
