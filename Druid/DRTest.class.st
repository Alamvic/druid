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

{ #category : #SCCP }
DRTest >> sccpEvaluateFor: sccp [
	
	| operand1 operand2 |
	operand1 := sccp value: self operand1.
	operand2 := sccp value: self operand2.
	
	((({ sccp top. sccp bottom } includes: operand1) not
		and: [ ({ sccp top. sccp bottom } includes: operand2) not ])
			and: [ operand1 isConstant and: [ operand2 isConstant ]])
				ifTrue: [ ^ operand1 value anyMask: operand2 value ].
	
	operand1 = sccp bottom ifTrue: [ ^ sccp bottom ].
	operand2 = sccp bottom ifTrue: [ ^ sccp bottom ].
	
	^ sccp top
]
