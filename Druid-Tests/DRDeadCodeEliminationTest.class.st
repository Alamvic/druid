Class {
	#name : #DRDeadCodeEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnConditionalJump [


	| cfg copy1 copy2 copy3 copy4 copy5 copy6 jump add1 add2 phi|
	optimisation := DRDeadCodeElimination new. 

	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRTemporaryRegister id: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRTemporaryRegister id: 2).

	jump := DRBranchIfTrue operands: { DRConstantValue value: true  }.

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
	cfg b4 addInstruction: (DRStore new operands: {
		"size"    8 asDRValue. 
		"value"   phi.
		"address" 17 asDRValue }). 
	
	optimisation applyTo: cfg.


	"Should eliminate R1:= and R2:="
	self assert: cfg instructions size equals: 12.
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnDeadEndInstruction [

	| cfg copy1 copy2 phi1|
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 2. 
	
	cfg b1 addInstruction: (DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	cfg b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)). 
	cfg b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRTemporaryRegister id: 3)). 
	
	phi1 := ((DRPhiFunction joinedVariables: { copy2. copy1 })
		result: (DRTemporaryRegister id: 4);
		yourself).
	cfg b2 addInstruction: phi1. 
	cfg b2 addInstruction: (DRStore new operands: {
		"size"    8 asDRValue. 
		"value"   phi1.
		"address" 17 asDRValue }).

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size equals: 6. "4 + Jump + Noop" 
	self assert: cfg instructions first equals: copy1. 
]

{ #category : #test }
DRDeadCodeEliminationTest >> testDCEOnSeveralBlocs [

	| b1 b2 cfg copy1 |
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 2. 
	
	b1 := cfg b1.
	b2 := cfg b2.

	b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)). 
	
	b2 addInstruction: (DRStore new operands: {
		"size"    8 asDRValue. 
		"value"   copy1.
		"address" 17 asDRValue }). 

	optimisation applyTo: cfg.
	
	self assert: cfg instructions size equals: 4.
]

{ #category : #test }
DRDeadCodeEliminationTest >> testRemoveUnusedNoop [

	| cfg |
	
	optimisation := DRDeadCodeElimination new. 
	
	cfg := self setUpCFG: 2. 
	
	cfg b2 addInstruction: (DRNoop new result: DRNoRegister new). 
	
	optimisation applyTo: cfg.

	self assert: cfg instructions size equals: 2.
	self assert: cfg instructions first isJump. "1 -> 2"
	self assert: cfg instructions second isNoop. "End instruction"
]
