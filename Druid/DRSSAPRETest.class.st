Class {
	#name : #DRSSAPRETest,
	#superclass : #DROptimisationTest,
	#instVars : [
		'instructionFactory',
		'valueNumberer'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRSSAPRETest >> setUp [

	super setUp.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRSSAPRE new.
	valueNumberer := DRValueNumberer new.
	
]

{ #category : #tests }
DRSSAPRETest >> testPartialRedundancyWithSameConstIn124BlockInConditionCF [
	"
T1 = e  → T2 = e  →   T4 = e
      \→  T3 = p /
⇒ 
T1 = e  → T2 = T1  →  T4 = T1
       \→  T3 = p /
"

	| cfg instruction1 instruction2 instruction3 instruction4 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b3 copy: 1.
	instruction4 := cfg b4 copy: 3.

	optimisation applyTo: cfg.

	self assert: cfg b1 first isCopy.
	self assert: cfg b2 first operand1 equals: instruction1. 
	self assert: cfg b3 first operand1 value equals: 1.
	self assert: cfg b4 first operand1 value equals: instruction1.
]

{ #category : #tests }
DRSSAPRETest >> testPartialRedundancyWithSameConstIn12BlockInConditionCF [
	"
T1 = e → T2 = e  →   T4 
      \→  T3 = p /
⇒ 
T1 = e  → T2 = T1  →  T4
       \→  T3 = p /
"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFGWithConditional: (instructionFactory jumpIf:
			        (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b3 copy: 1.
	instruction3 := cfg b2 copy: 3.

	optimisation applyTo: cfg.


	self assert: cfg b1 first isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	self assert: cfg b3 first operand1 value equals: 1.
]
