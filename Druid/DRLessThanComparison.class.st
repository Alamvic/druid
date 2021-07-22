Class {
	#name : #DRLessThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitLessThanComparison: self
]

{ #category : #visiting }
DRLessThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	^ #JumpLess:
]

{ #category : #printing }
DRLessThanComparison >> opcode [
	
	^ 'LessThan'
]

{ #category : #printing }
DRLessThanComparison >> reversed [
	
	^ DRGreaterOrEqualsThanComparison operands: operands reversed result: result
]
