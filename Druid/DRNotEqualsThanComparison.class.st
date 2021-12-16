Class {
	#name : #DRNotEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRNotEqualsThanComparison >> acceptVisitor: aVisitor [
	
	^ aVisitor visitNotEqualsComparison: self
]

{ #category : #'rtl-translation' }
DRNotEqualsThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpZero:
]

{ #category : #'rtl-translation' }
DRNotEqualsThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNonZero:
]

{ #category : #printing }
DRNotEqualsThanComparison >> opcode [
	
	^ 'NotEqualsThan'
]

{ #category : #printing }
DRNotEqualsThanComparison >> reversed [
	
	^ DREqualsThanComparison operands: operands reversed result: result
]
