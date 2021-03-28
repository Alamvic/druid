Class {
	#name : #DRLessOrEqualsComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessOrEqualsComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitLessOrEqualsComparison: self
]
