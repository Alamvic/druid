Class {
	#name : 'DRDeadCodeEliminationTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'test' }
DRDeadCodeEliminationTest >> setUp [

	super setUp.
	optimisation := DRDeadCodeElimination new
]

{ #category : 'tests' }
DRDeadCodeEliminationTest >> testDCEOnConditionalJump [

	| cfg copy1 copy2 copy3 copy4 copy5 copy6 jump add1 add2 phi instructionsBefore |
	copy1 := DRCopy
		         operands: { (DRConstantValue value: 1) }
		         result: (DRSSARegister name: 1).
	copy2 := DRCopy
		         operands: { (DRConstantValue value: 2) }
		         result: (DRSSARegister name: 2).

	jump := self newConditionalJump.
	cfg := self setUpCFGWithConditional: jump.

	cfg b1 addInstruction: copy1.
	cfg b1 addInstruction: copy2.

	cfg b2 addInstruction: (copy3 := DRCopy
			          operands: { (DRConstantValue value: 3) }
			          result: (DRSSARegister name: 3)).
	cfg b2 addInstruction: (copy4 := DRCopy
			          operands: { (DRConstantValue value: 4) }
			          result: (DRSSARegister name: 4)).
	cfg b2 addInstruction:
		(add1 := DRAdd operands: {copy3. copy4} result: (DRSSARegister name: 7)).

	cfg b3 addInstruction: (copy5 := DRCopy
			          operands: { (DRConstantValue value: 5) }
			          result: (DRSSARegister name: 5)).
	cfg b3 addInstruction: (copy6 := DRCopy
			          operands: { (DRConstantValue value: 6) }
			          result: (DRSSARegister name: 6)).
	cfg b3 addInstruction:
		(add2 := DRAdd operands: { copy5. copy6 } result: (DRSSARegister name: 8)).
 
	cfg b4 addInstruction: (phi := ((DRPhiFunction joinedVariables: { add1. add2 }) result: (DRSSARegister name: 9); yourself)).
	cfg b4 addInstruction: (DRStore new operands: {
		"size"    8 asDRValue. 
		"value"   phi.
		"address" 17 asDRValue }). 
	
	instructionsBefore := cfg instructions size.
	optimisation applyTo: cfg.


	"Should eliminate R1:= and R2:="
	self assert: cfg instructions size equals: instructionsBefore - 2.
]

{ #category : 'tests' }
DRDeadCodeEliminationTest >> testDCEOnDeadEndInstruction [

	| cfg copy1 copy2 phi1 instructionsBefore |	
	cfg := self setUpCFG: 2. 
	
	cfg b1 addInstruction: (DRCopy operands: { DRConstantValue value: 1 } result:  (DRSSARegister name: 1)). 
	cfg b1 addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRSSARegister name: 2)). 
	cfg b1 addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 3 } result:  (DRSSARegister name: 3)). 
	
	phi1 := ((DRPhiFunction joinedVariables: { copy2. copy1 })
		result: (DRSSARegister name: 4);
		yourself).
	cfg b2 addInstruction: phi1. 
	cfg b2 addInstruction: (DRStore new operands: {
		"size"    8 asDRValue. 
		"value"   phi1.
		"address" 17 asDRValue }).

	instructionsBefore := cfg instructions size.
	optimisation applyTo: cfg.
	
	self assert: cfg instructions size equals: instructionsBefore - 1.
	self assert: cfg instructions first equals: copy1. 
]

{ #category : 'tests' }
DRDeadCodeEliminationTest >> testDCEOnSeveralBlocs [

	| b1 b2 cfg copy1 instructionsBefore |
	cfg := self setUpCFG: 2.

	b1 := cfg b1.
	b2 := cfg b2.

	b1 copy: 1.

	b2 storeSInt64: copy1 at: 17 asDRValue.

	instructionsBefore := cfg instructions size.
	optimisation applyTo: cfg.

	self assert: cfg instructions size equals: instructionsBefore - 1
]

{ #category : 'test' }
DRDeadCodeEliminationTest >> testNotRemoveUnusedMandatoryInstruction [

	| cfg mandatory addition |
	cfg := self setUpCFG: 2.

	mandatory := cfg b2 storeSInt64: 1 asDRValue at: 999.
	addition := cfg b2 add: mandatory to: 1.

	optimisation applyTo: cfg.

	self denyCollection: cfg instructions includesAny: { addition }.
	self assertCollection: cfg instructions includesAny: { mandatory }
]

{ #category : 'tests' }
DRDeadCodeEliminationTest >> testRemoveUnusedNoop [

	| cfg instructionsBefore |
	cfg := self setUpCFG: 2. 
	
	cfg b2 addInstruction: (DRNoop new result: DRNoRegister new). 

	instructionsBefore := cfg instructions size.
	optimisation applyTo: cfg.

	self assert: cfg instructions size equals: instructionsBefore - 1.
	self assert: cfg instructions first isJump. "1 -> 2"
]
