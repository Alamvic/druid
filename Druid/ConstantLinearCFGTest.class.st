Class {
	#name : #ConstantLinearCFGTest,
	#superclass : #DRSSAPRETest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
ConstantLinearCFGTest >> testFullRedundancyCaseWith3ConstExpressInSingleBlock [
	"case B: full redundancy
	T1 = 3
	T2 = 3
	T3 = 3
	
	⇒
	
	T1 = 3
	T2 = T1
	T3 = T1"

	| cfg instruction1 instruction2 instruction3 |
	cfg := self setUpCFG: 1.
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b1 copy: 3.
	instruction3 := cfg b1 copy: 3.


	optimisation applyTo: cfg.

	self assert: cfg b1 third isCopy.
	self assert: cfg b1 third operand1 equals: instruction1
]

{ #category : #tests }
ConstantLinearCFGTest >> testFullRedundancyCaseWithAlternateConstExpressInLinearCF [
	"case C: full redundancy linear CF with alternate instructions
		T1 = e → T2 = e2 → T3 = e → T4= e2"

	| cfg instruction1 instruction2 instruction3 |
	
	cfg := self setUpCFG: 3.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b2 copy: 3.
	instruction3 := cfg b3 copy: 1.

	optimisation applyTo: cfg.
	
	self assert: cfg b1 first isCopy.
	self assert: cfg b2 first isCopy.
	self assert: cfg b3 first operand1 equals: instruction1.

]

{ #category : #tests }
ConstantLinearCFGTest >> testFullRedundancyCaseWithConstantInSingleBlock [
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
ConstantLinearCFGTest >> testFullRedundancyCaseWithMultipleConstExpressInLinearCF [
	"case C: full redundancy linear CF with multiple instructions
	T1 = e 
	T2 = e2
	→ 
	T3 = e
	T4 = e2
		⇒    
	T1 = e 
	T2 = e2
	→ 
	T3 = T1
	T4 = T2 "

	| cfg instruction1 instruction2 instruction3 instruction4 |
	
	cfg := self setUpCFG: 2.
	instruction1 := cfg b1 copy: 1.
	instruction2 := cfg b1 copy: 3.
	instruction3 := cfg b2 copy: 1.
	instruction4 := cfg b2 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 second isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	self assert: cfg b2 second operand1 equals: instruction2.
]

{ #category : #tests }
ConstantLinearCFGTest >> testFullRedundancyCaseWithSingleConstExpressInLinearCF [
	"case C: full redundancy linear CF with single instruction
	T1 = e → T2 = e
	⇒ 
	T1 = e → T2 = T1 "

	| cfg instruction1 instruction2 |
	
	cfg := self setUpCFG: 2.
	
	instruction1 := cfg b1 copy: 3.
	instruction2 := cfg b2 copy: 3.
	
	optimisation applyTo: cfg.
	
	self assert: cfg b2 first isCopy.
	self assert: cfg b2 first operand1 equals: instruction1.
	
]

{ #category : #tests }
ConstantLinearCFGTest >> testNoRedundancyCaseWithConstantInDiffBlock [
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
