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
DRDeadCodeEliminationTest >> setUpCFGConditionnal: jump [

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4. 
	
	cfg initialBasicBlock: b0.
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1. 
	b1 addInstruction: jump condition. 
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	b2 addPredecessor: b1. 
	jump falseBranch: b3.
	b2 addPredecessor: b1. 
	b2 jumpTo: b4. 
	b3 jumpTo: b4. 
		
	cfg b0: b0.
	cfg b1: b1. 
	cfg b2: b2. 
	cfg b3: b3. 
	cfg b4: b4. 
	cfg conditionnalJump: jump. 

	^ cfg. 



















]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnConditionalJump [


	| cfg b0 b1 b2 b3 b4 optimisation copy1 copy2 copy3 copy4 copy5 copy6 jump b2OperandList b3OperandList add1 add2 phi|
	optimisation := DRDeadCodeElimination new. 

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 2).
	jump := DRConditionalJump operands: { DRConstantValue value: true  }.

	cfg := self setUpCFGConditionnal: jump.
	b0 := cfg b0.
	b1 := cfg b1.
	b2 := cfg b2.
	b3 := cfg b3.
	b4 := cfg b4.


	b1 addInstruction: copy1.
	b1 addInstruction: copy2.

	b2 addInstruction: (copy3 := DRCopy
			          operands: { (DRConstantValue value: 3) }
			          result: (DRTemporaryRegister id: 3)).
	b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRTemporaryRegister id: 4)).
	b2OperandList := OrderedCollection new.
	b2OperandList add: copy3.
	b2OperandList add: copy4.
	b2 addInstruction:
		(add1 := DRAdd operands: b2OperandList result: (DRTemporaryRegister id: 7)).

	b3 addInstruction: (copy5 := DRCopy
			          operands: { (DRConstantValue value: 5) }
			          result: (DRTemporaryRegister id: 5)).
	b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRTemporaryRegister id: 6)).
	b3OperandList := OrderedCollection new.
	b3OperandList add: copy5.
	b3OperandList add: copy6.
	b3 addInstruction:
		(add2 := DRAdd operands: b3OperandList result: (DRTemporaryRegister id: 8)).
 
	b4 addInstruction: (phi := ((DRPhiFunction joinedVariables: { add1. add2 }) result: (DRTemporaryRegister id: 9); yourself)).

	b4 addInstruction: (DRStore new operands: {phi}). 
	optimisation applyTo: cfg.


	self assert: cfg instructions size equals: 13.
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
		result: (DRTemporaryRegister id: 4);
		yourself).
	b0 addInstruction: phi1. 
	b0 addInstruction: (DRStore new operands: {phi1} ). 

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size = 5. 
	self assert: cfg instructions first equals: copy2. 
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnSeveralBlocs [

	| optimisation b0 b1 cfg copy1 |
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 2. 
	
	b0 := cfg b0. 
	b1 := cfg b1. 
	
	b0 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	
	b1 addInstruction: (DRStore new operands: {copy1} ). 

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size = 4.
]
