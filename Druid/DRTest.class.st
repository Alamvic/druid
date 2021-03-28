Class {
	#name : #DRTest,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRTest >> acceptVisitor: aVisitor [
	
	^ aVisitor visitTest: self
]
