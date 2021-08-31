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

{ #category : #SCCP }
DRLessThanComparison >> sccpEvaluateFor: sccp [
	
	| operand1 operand2 |
	operand1 := sccp value: self operand1.
	operand2 := sccp value: self operand2.
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value < operand2 value ].
	
	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	operand2 = sccp bottom ifTrue: [ ^ sccp bottom ].
	
	^ sccp top
]
