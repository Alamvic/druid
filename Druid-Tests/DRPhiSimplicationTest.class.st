Class {
	#name : #DRPhiSimplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #running }
DRPhiSimplicationTest >> setUp [ 

	super setUp.
	optimisation := DRPhiSimplication new
]

{ #category : #tests }
DRPhiSimplicationTest >> testNotSimplifyDualPhi [

	| cfg t1 t2 phi |
	cfg := self setUpCFGWithConditional.
	
	t1 := cfg b2 copy: 1.
	t2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: t1 with: t2.

	DRPhiSimplication new applyTo: cfg.

	self assert: cfg b4 firstInstruction equals: phi
]

{ #category : #tests }
DRPhiSimplicationTest >> testNotSimplifyPhiWithNull [

	| cfg  t1 block phi |
	cfg := self setUpCFGWithConditional.
	block := cfg b4.
		
	t1 := cfg b2 copy: 1.
	phi := block phiWith: t1.

	cfg applyOptimisation: optimisation.

	self assert: phi operands size equals: 2.
	self assert: phi operands last isNullValue.

]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifyPhiOnlyWithNulls [

	| cfg |
	cfg := self setUpCFG: 1.

	cfg b1 phiWithVariables: {
			DRNullValue new.
			DRNullValue new }.

	DRPhiSimplication new applyTo: cfg.

	self assert: cfg b1 firstInstruction isCopy.
	self assert: cfg b1 firstInstruction operand1 isNullValue
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifyRecursivePhi [

	| cfg t1 block phi |
	cfg := self setUpCFGWithConditional.
	block := cfg b4.

	t1 := cfg b1 copy: 1.
	phi := block phiWith: t1.
	phi replaceOperandAtIndex: 2 by: phi.

	self assert: phi operands size equals: 2.
	self assertCollection: phi operands hasSameElements: { 
			t1.
			phi }.

	cfg applyOptimisation: optimisation.

	self assert: block firstInstruction isCopy.
	self assert: block firstInstruction operand1 equals: t1
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifyRedundantPhi [

	| cfg t1 block phi |
	cfg := self setUpCFGWithConditional.
	block := cfg b4.

	t1 := cfg b1 copy: 1.
	phi := block phiWith: t1 with: t1.

	self assert: phi operands size equals: 2.
	self assertCollection: phi operands hasSameElements: { 
			t1.
			t1 }.

	cfg applyOptimisation: optimisation.

	self assert: block firstInstruction isCopy.
	self assert: block firstInstruction operand1 equals: t1
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifyRedundantPhiFromDifferentCopies [

	| cfg t1 t2 t3 block phi |
	"Pattern not supported yet"
	self skip.

	cfg := self setUpCFGWithConditional.
	block := cfg b4.

	t1 := cfg b1 copy: 1.
	t2 := cfg b2 copy: t1.
	t3 := cfg b3 copy: t1.
	phi := block phiWith: t2 with: t3.

	self assert: phi operands size equals: 2.
	self assertCollection: phi operands hasSameElements: {
			t2.
			t3 }.

	cfg applyOptimisation: optimisation.

	self assert: block firstInstruction isCopy.
	self assert: block firstInstruction operand1 equals: t1
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifyRedundantPhiFromDifferentPhis [

	| cfg t1 t2 t3 block phi |
	"Pattern not supported yet"
	self skip.

	cfg := self setUpCFGWithConditional.
	block := cfg b4.

	t1 := cfg b1 copy: 1.
	t2 := cfg b2 phiWith: t1.
	t3 := cfg b3 phiWith: t1.
	phi := block phiWith: t2 with: t3.

	self assert: phi operands size equals: 2.
	self assertCollection: phi operands hasSameElements: {
			t2.
			t3 }.

	cfg applyOptimisation: optimisation.

	self assert: block firstInstruction isCopy.
	self assert: block firstInstruction operand1 equals: 1 asDRValue
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplifySimplePhi [

	| cfg t1 phi block |
	cfg := self setUpCFG: 2.
	block := cfg b2.

	t1 := cfg b1 copy: 1.
	phi := cfg b2 phiWith: t1.

	self assert: phi operands size equals: 1.
	self assertCollection: phi operands hasSameElements: { t1 }.

	DRPhiSimplication new applyTo: cfg.

	self assert: block firstInstruction isCopy.
	self assert: block firstInstruction operand1 equals: t1
]
