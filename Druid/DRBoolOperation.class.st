Class {
	#name : #DRBoolOperation,
	#superclass : #DRInstruction,
	#category : #'Druid-IR'
}

{ #category : #'as yet unclassified' }
DRBoolOperation >> sccpLatticeValueOperand: index for: sccp [

	| value |
	value := (self operands at: index) sccpLatticeValueFor: sccp.
	value = sccp top ifTrue: [ ^ self operand2 ].
	^ value
]
