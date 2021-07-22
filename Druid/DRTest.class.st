Class {
	#name : #DRTest,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRTest >> acceptVisitor: aVisitor [
	
	^ aVisitor visitTest: self
]

{ #category : #visiting }
DRTest >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking equals, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpZero:
]

{ #category : #printing }
DRTest >> opcode [
	
	^ 'Test'
]

{ #category : #converting }
DRTest >> reversed [
	
	^ DRTest operands: operands reversed result: result
]
