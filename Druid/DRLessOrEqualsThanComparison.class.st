Class {
	#name : #DRLessOrEqualsThanComparison,
	#superclass : #DRCondition,
	#category : #'Druid-IR'
}

{ #category : #visiting }
DRLessOrEqualsThanComparison >> acceptVisitor: aVisitor [ 

	^ aVisitor visitLessOrEqualsThanComparison: self
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> cogitTrueConditionCode [
	
	"Return the condition code used to jump if this condition is false.
	Since we are checking lessOrEquals, the we should return greaterThan"
	
	^ #JumpLessOrEqual:
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> opcode [
	
	^ 'LessOrEqualsThan'
]

{ #category : #printing }
DRLessOrEqualsThanComparison >> reversed [
	
	^ DRGreaterThanComparison operands: operands reversed result: result
]

{ #category : #SCCP }
DRLessOrEqualsThanComparison >> sccpEvaluateFor: sccp [
	
	| operand1 operand2 |
	operand1 := sccp value: self operand1.
	operand2 := sccp value: self operand2.
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value <= operand2 value ].
	
	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	operand2 = sccp bottom ifTrue: [ ^ sccp bottom ].
	
	^ sccp top
]
