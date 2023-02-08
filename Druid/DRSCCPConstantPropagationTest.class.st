Class {
	#name : #DRSCCPConstantPropagationTest,
	#superclass : #DRSCCPTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPConstantPropagationTest >> testDoNotPropagateInReturn [

	| cfg copy return |
	cfg := self setUpCFG: 1.

	copy := cfg b1 copy: 1.
	return := cfg b1 return: copy.

	optimisation applyTo: cfg.

	self assert: return operand1 equals: copy
]
