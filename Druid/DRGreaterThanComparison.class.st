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
DRGreaterThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking greaterOrEquals, the we should return lessThan"
	
	^ #JumpGreater:
]

{ #category : #printing }
DRGreaterThanComparison >> opcode [
	
	^ 'GreaterThan'
]

{ #category : #reversing }
DRGreaterThanComparison >> reversed [
	
	^ DRLessOrEqualsThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DRGreaterThanComparison >> sccpLatticeValueFor: sccp [
	
	| operand1 operand2 |
	operand1 := sccp latticeValue: self operand1.
	operand2 := sccp latticeValue: self operand2.
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value > operand2 value ].
	
	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	operand2 = sccp bottom ifTrue: [ ^ sccp bottom ].
	
	^ sccp top
]
