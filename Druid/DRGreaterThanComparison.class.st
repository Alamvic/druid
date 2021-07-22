Class {
	#name : #DRGreaterThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGreaterThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitGreaterThanComparison: self
]

{ #category : #visiting }
DRGreaterThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpLessOrEqual:
]

{ #category : #printing }
DRGreaterThanComparison >> opcode [
	
	^ 'GreaterThan'
]
