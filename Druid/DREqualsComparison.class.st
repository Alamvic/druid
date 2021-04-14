Class {
	#name : #DREqualsComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DREqualsComparison >> acceptVisitor: aVisitor [
	
	^ aVisitor visitEqualsComparison: self
]

{ #category : #printing }
DREqualsComparison >> opcode [
	
	^ 'Equals'
]
