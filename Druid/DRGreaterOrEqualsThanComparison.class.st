Class {
	#name : #DRGreaterOrEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRGreaterOrEqualsThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitGreaterOrEqualsThanComparison: self
]

{ #category : #visiting }
DRGreaterOrEqualsThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpLess:
]

{ #category : #printing }
DRGreaterOrEqualsThanComparison >> opcode [
	
	^ 'GreaterOrEquals'
]

{ #category : #reversing }
DRGreaterOrEqualsThanComparison >> reversed [
	
	^ DRLessThanComparison operands: operands reversed result: result
]
