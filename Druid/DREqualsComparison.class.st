Class {
	#name : #DREqualsComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DREqualsComparison >> acceptVisitor: aVisitor [
	
	^ aVisitor visitEqualsComparison: self
]

{ #category : #'rtl-translation' }
DREqualsComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNonZero:
]

{ #category : #printing }
DREqualsComparison >> opcode [
	
	^ 'Equals'
]
