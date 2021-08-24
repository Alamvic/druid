Class {
	#name : #DRDeadCodeEliminationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #test }
DRDeadCodeEliminationTest >> setUpCFG: n [

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	
	cfg initialBasicBlock: b0.
	
	n = 1 ifTrue: [ 
		b0 endInstruction: DRNoop new.
		cfg initialBasicBlock: b0. 
		cfg b0: b0. ].
	
	n = 2 ifTrue: [ 
		b1 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		
		cfg b0: b0. 
		cfg b1: b1. ].

	n = 3 ifTrue: [ 
		b2 endInstruction: DRNoop new.
		b0 jumpTo: b1.
		b1 jumpTo: b2. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. ].
	
	n = 4 ifTrue: [ 
		b3 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		b1 jumpTo: b2. 
		b2 jumpTo: b3. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. 
		cfg b3: b3. ].

	^ cfg.
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnDeadEndInstruction [

	| optimisation b0 cfg copy1 copy2 copy3 phi1|
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 1. 
	
	b0 := cfg b0. 
	
	b0 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	b0 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)). 
	b0 addInstruction: (copy3 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 3)). 
	
	phi1 := ((DRPhiFunction joinedVariables: { copy3. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
	b0 addInstruction: phi1. 
	b0 addInstruction: (DRStore new operands: {phi1} ). 
	
	optimisation applyTo: cfg.
	
	self assert: cfg instructions size = 5. 
	self assert: cfg instructions first equals: copy2. 
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnSameInstruction [

	| optimisation b0 cfg copy1 |
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 1. 
	
	b0 := cfg b0. 
	
	b0 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	b0 addInstruction: copy1. 
	b0 addInstruction: (DRStore new operands: {copy1} ). 
	
	optimisation applyTo: cfg.
	
	self assert: cfg instructions size = 3. 
	self assert: cfg instructions first equals: copy1. 
]
