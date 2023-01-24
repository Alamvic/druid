Class {
	#name : #DRSSAPRETest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #running }
DRSSAPRETest >> setUp [

	super setUp.
	optimisation := DRSSAPRE new.
	
]

{ #category : #tests }
DRSSAPRETest >> testFullRedundancyCaseWithConstantInSingleBlock [
	"case B: full redundancy
	T1 = 3
	T2 = 3
	
	⇒
	
	T1 = 3
	T2 = T1"

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 1.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b1 copy: 3.

	optimisation applyTo: cfg.

	self assert: cfg b1 second isCopy.
	self assert: cfg b1 second operand1 equals: instruction1.
]

{ #category : #tests }
DRSSAPRETest >> testNoRedundancyCaseWithConstantInDiffBlock [
	"case A: no redundancy
	T1 = 3
	->
	T2 = 2
	
	⇒ do nothing"

	| cfg instruction1 instruction2 |
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 2.

	optimisation applyTo: cfg.

	self assert: cfg b1 first isCopy.
	self assert: cfg b1 first operand1 value equals: 3.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 first operand1 value equals: 2.
]
