Class {
	#name : #DROr,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DROr >> acceptVisitor: aVisitor [

	^ aVisitor visitOr: self
]

{ #category : #visiting }
DROr >> cogitFalseConditionCode [
	
	"Jump false if the constant is a zero"
	^ #JumpZero:
]

{ #category : #visiting }
DROr >> cogitTrueConditionCode [

	"Return the condition code used to jump if this condition is true.
	Since we are checking OR, the we should return non-equals, represented as a NonZero in cogit rtl"
	
	^ #JumpNonZero:
]

{ #category : #printing }
DROr >> opcode [
	
	^ 'OR'
]

{ #category : #SCCP }
DROr >> sccpLatticeValueFor: sccp [

	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1. 
	operand2 := sccp latticeValue: self operand2.
	
	(operand1 value = true
		or: [ operand2 value = true ])
			ifTrue: [ ^ true asDRValue ].
	
	^ super sccpLatticeValueFor: sccp
]

{ #category : #SCCP }
DROr >> sccpOperateOnLattice: aBoolean and: anotherBoolean [

	^ aBoolean or: anotherBoolean
]
