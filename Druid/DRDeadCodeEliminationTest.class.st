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
DRDeadCodeEliminationTest >> setUpCFGWithConditional: jump [

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
	jump condition isInstruction ifTrue: [
		b1 addInstruction: jump condition. 
	].
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


	| cfg optimisation copy1 copy2 copy3 copy4 copy5 copy6 jump add1 add2 phi|
	optimisation := DRDeadCodeElimination new. 

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 2).
	jump := DRConditionalJump operands: { DRConstantValue value: true  }.

	cfg := self setUpCFGWithConditional: jump.

	cfg b1 addInstruction: copy1.
	cfg b1 addInstruction: copy2.

	cfg b2 addInstruction: (copy3 := DRCopy
			          operands: { (DRConstantValue value: 3) }
			          result: (DRTemporaryRegister id: 3)).
	cfg b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRTemporaryRegister id: 4)).
	cfg b2 addInstruction:
		(add1 := DRAdd operands: {copy3. copy4} result: (DRTemporaryRegister id: 7)).

	cfg b3 addInstruction: (copy5 := DRCopy
			          operands: { (DRConstantValue value: 5) }
			          result: (DRTemporaryRegister id: 5)).
	cfg b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRTemporaryRegister id: 6)).
	cfg b3 addInstruction:
		(add2 := DRAdd operands: { copy5. copy6 } result: (DRTemporaryRegister id: 8)).
 
	cfg b4 addInstruction: (phi := ((DRPhiFunction joinedVariables: { add1. add2 }) result: (DRTemporaryRegister id: 9); yourself)).
	cfg b4 addInstruction: (DRStore new operands: {phi}). 
	
	optimisation applyTo: cfg.


	"Should eliminate R1:= and R2:="
	self assert: cfg instructions size equals: 12.
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnDeadEndInstruction [

	| optimisation b1 cfg copy1 copy2 copy3 phi1|
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 2. 
	
	b1 := cfg b1.
	
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)). 
	b1 addInstruction: (copy3 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 3)). 
	
	phi1 := ((DRPhiFunction joinedVariables: { copy3. copy2 })
		result: (DRTemporaryRegister id: 4);
		yourself).
	b1 addInstruction: phi1. 
	b1 addInstruction: (DRStore new operands: {phi1} ). 

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size equals: 5. 
	self assert: cfg instructions first equals: copy2. 
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnSeveralBlocs [

	| optimisation b1 b2 cfg copy1 |
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 3. 
	
	b1 := cfg b1.
	b2 := cfg b2.
	
	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	
	b2 addInstruction: (DRStore new operands: {copy1} ). 

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size equals: 4.
]
