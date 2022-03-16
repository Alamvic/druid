Class {
	#name : #DREqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DREqualsThanComparison >> acceptVisitor: aVisitor [
	
	^ aVisitor visitEqualsComparison: self
]

{ #category : #'rtl-translation' }
DREqualsThanComparison >> cogitFalseConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNonZero:
]

{ #category : #'rtl-translation' }
DREqualsThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpZero:
]

{ #category : #printing }
DREqualsThanComparison >> opcode [
	
	^ 'EqualsThan'
]

{ #category : #printing }
DREqualsThanComparison >> reversed [
	
	^ DREqualsThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DREqualsThanComparison >> sccpOperateOnLattice: operand and: operand2 [
	
	^ operand = operand2
]
