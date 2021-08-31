Class {
	#name : #DRLessOrEqualsComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #printing }
DRLessOrEqualsComparison >> value [ 

	^ self operand1 operand1 value <= self operand2 operand1 value
]
