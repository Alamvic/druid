Class {
	#name : #DRBooleanInstruction,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRBooleanInstruction >> isComplexCondition [

	^ true
]

{ #category : #SCCP }
DRBooleanInstruction >> sccpLatticeValueOperand: index for: sccp [

	| operand value |
	operand := self operands at: index.
	value := operand sccpLatticeValueFor: sccp.
	value = sccp top ifTrue: [ ^ operand ].
	^ value
]
