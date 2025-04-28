Class {
	#name : 'DRPiNodesTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'running' }
DRPiNodesTest >> insertPiNodes: cfg [
	cfg applyOptimisation: DRCogitCanonicaliser new. 
	cfg applyOptimisation: DRInsertPiNodes new.

]

{ #category : 'running' }
DRPiNodesTest >> optimize: cfg with: optimisations [

	self insertPiNodes: cfg.
	
	cfg applyOptimisation: ((optimisations collect: [ :o | o new ]) reduce: [ :o1 :o2 | o1 then: o2 ]).
	
	cfg applyOptimisation: DRCopyPropagation new.
	cfg applyOptimisation: DRDeadCodeElimination new.
]

{ #category : 'running' }
DRPiNodesTest >> piNodesIn: aDRBasicBlock [

	^ aDRBasicBlock instructions select: [ :i | i isPiNode ]
]

{ #category : 'running' }
DRPiNodesTest >> setUpCFGWithConditionalBetweenVars: aBlock [

	| var1 var2 cfg instructionFactory cond jumpIf realCond |
	
	instructionFactory := DRInstructionFactory new.
	cond := instructionFactory equals: 1 to: 2.
	jumpIf := instructionFactory jumpIf: cond.
	cfg := self setUpCFGWithConditional: jumpIf.
	
	instructionFactory := cfg instructionFactory.
	var1 := instructionFactory loadFramePointer.
	var2 := instructionFactory loadFramePointer.

	realCond := (aBlock value: cfg value: var1 value: var2).
	cfg b1 replaceInstruction: cond by: realCond.
	cfg b1 replaceInstruction: jumpIf by: ((instructionFactory jumpIf: realCond)
		trueBranch: cfg b2;
		falseBranch: cfg b3;
		yourself).
		
	cfg b0 addInstructionFirst: var1.
	cfg b0 addInstructionFirst: var2.
	
	cfg b2 storeSInt64: var1 at: 777.
	cfg b3 storeSInt64: var1 at: 888.
	cfg b4 storeSInt64: var2 at: 999.

	cfg validate.

	^ cfg.
	
]

{ #category : 'tests' }
DRPiNodesTest >> testABCDGraph [
	| cfg b_i b_end limit0 limit1 st0 st1 while b_for j1 b_if1 st3 limit3 j0 b_if2 t0 b_if2_after_check2 b_if2_after_check1 j4 cmp_for dummyOperand1 jmp_for |
	
	"This is the running example in the ABCD paper, reproduced it just to have something to validate our results agains
	The code is kinda ugly, I don't know if there's a nicer way to build this self-referencial/cyclic IRs"

	cfg := DRControlFlowGraph new.
	
	b_end := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
		
	b_i := cfg newBasicBlockWith: [ :block |
		dummyOperand1 := block copy: 1234.
		limit0 := block loadFramePointer.
		st0 := block copy: -1.
	].
	cfg initialBasicBlock jumpTo: b_i.
	
	b_if2_after_check2 := cfg newBasicBlockWith: [ :block |
		j4 := block add: 10 to: dummyOperand1 "j1".
	].

	b_if2_after_check1 := cfg newBasicBlockWith: [ :block | |check|
		t0 := block add: 11 to: dummyOperand1 "j1".
		check := block less: t0 than: limit0.
		block jumpIf: check to: b_if2_after_check2 ifFalseTo: b_end.
	].
	b_if2 := cfg newBasicBlockWith: [ :block | |check|
		check := block less: dummyOperand1 "j1" than: limit0.
		block jumpIf: check to: b_if2_after_check1 ifFalseTo: b_end.
	].

	b_for := cfg newBasicBlockWith: [ :block | ].

	b_if2_after_check2 jumpTo: b_for.
	
	cmp_for := b_for less: dummyOperand1 than: limit0.
	jmp_for := b_for jumpIf: cmp_for to: b_if2 ifFalseTo: b_i.

	j1 := b_for phiWithVariables: {1234 asDRValue . 4567 asDRValue }.
	cmp_for replaceOperand: dummyOperand1 by: j1.
	
	b_if2_after_check2 instructions first replaceOperand: dummyOperand1 by: j1.
	b_if2_after_check1 instructions first replaceOperand: dummyOperand1 by: j1.
	b_if2 instructions first replaceOperand: dummyOperand1 by: j1.

	b_if1 := cfg newBasicBlockWith: [ :block |
		st3 := block add: 12 to: dummyOperand1 "st1".
		limit3 := block subtract: 1 from: dummyOperand1 "limit1".
		j0 := block copy: st3.
		block jumpTo: b_for.
	].

	j1 replaceOperand: j1 operands first by: j0.
	j1 replaceOperand: j1 operands second by: j4.
		
	while := cfg newBasicBlockWith: [ :block | |cmp|
		limit1 := block phiWith: {limit0. limit3}.
		st1 := block phiWith: {st0. st3}.
		cmp := block less: st1 than: limit1.
		block jumpIf: cmp to: b_if1 ifFalseTo: b_end
	].

	b_if1 instructions first replaceOperand: dummyOperand1 by: st1.
	b_if1 instructions second replaceOperand: dummyOperand1 by: limit1.

	b_i jumpTo: while.

	jmp_for falseBranch removePredecessor: jmp_for basicBlock.
	jmp_for newFalseBranch: while.
	jmp_for basicBlock predecessors add: jmp_for basicBlock predecessors removeFirst.

	
	cfg validate.
	self insertPiNodes: cfg.

]

{ #category : 'tests' }
DRPiNodesTest >> testInsertPiNodesEqualsCondition [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].

	self insertPiNodes: cfg.
	
	self assert: (self piNodesIn: cfg b2) size equals: 2.
	self assert: ((self piNodesIn: cfg b2) allSatisfy: [ :pi | pi constraint class == DREqualsObjectConstraint ]).
	
	self assert: (self piNodesIn: cfg b3) size equals: 2.
	self assert: ((self piNodesIn: cfg b3) allSatisfy: [ :pi | pi constraint class == DRNotEqualsObjectConstraint ]).

]

{ #category : 'tests' }
DRPiNodesTest >> testInsertPiNodesRemoval [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].

	self optimize: cfg with: {DRNoOptimisation}.
	
	self assert: (self piNodesIn: cfg b2) size equals: 0.
	
	self assert: (self piNodesIn: cfg b3) size equals: 0.

]

{ #category : 'tests' }
DRPiNodesTest >> testPiInsertionBreaksCriticalEdges [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
			
	cfg b1 lastInstruction replaceTarget: cfg b2 by: cfg b4.
	cfg b4 addPredecessor: cfg b1.
	
	cfg b2 instructionsDo: [ :i | i removeFromCFG ].
	cfg removeBasicBlock: cfg b2.
	
	self insertPiNodes: cfg.
	
	self assert: cfg blocks size equals: 5.
	self assert: (cfg b1 successors includes: cfg b4) not.
	self assert: (cfg b1 successors includes: (cfg blocks detect: [ :b | b id == 5 ])).	
	
	self assert: ((cfg blocks detect: [ :b | b id == 5 ]) instructions anySatisfy: [ :i | i isPiNode ]).
	self assert: (cfg b4 instructions noneSatisfy: [ :i | i isPiNode ]).

]

{ #category : 'tests' }
DRPiNodesTest >> testUsagesInDominatedBlocksAreReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
	
	self insertPiNodes: cfg.
	
	store := cfg b2 instructions at: 3.
	self assert: store isStore.
	self assert: ((self piNodesIn: cfg b2) anySatisfy: [ :pi| pi == store operand1]).

]

{ #category : 'tests' }
DRPiNodesTest >> testUsagesInNonDominatedBlocksAreNotReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
	
	self insertPiNodes: cfg.
	
	store := cfg b4 instructions first.
	self assert: store isStore.
	
	self assert: ((self piNodesIn: cfg b2) noneSatisfy: [ :pi| pi result == store operand1]).
	self assert: ((self piNodesIn: cfg b3) noneSatisfy: [ :pi| pi result == store operand1]).
	
]
