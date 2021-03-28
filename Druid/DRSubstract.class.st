Class {
	#name : #DRSubstract,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRSubstract >> acceptVisitor: aVisitor [
	
	^ aVisitor visitSubstract: self
]
