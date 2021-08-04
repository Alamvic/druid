Class {
	#name : #DRLessOrEqualsComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessOrEqualsComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitLessOrEqualsComparison: self
]

{ #category : #printing }
DRLessOrEqualsComparison >> opcode [
	
	^ 'LessOrEquals'
]

{ #category : #printing }
DRLessOrEqualsComparison >> value [ 

	^ self operand1 operand1 value <= self operand2 operand1 value
]
