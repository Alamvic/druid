Class {
	#name : 'DRPiNodesInsertionTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiNodesInsertionTest >> testABCDGraph [
	| cfg b_i b_end limit0 limit1 st0 st1 b_while b_for j1 b_if1 st3 limit3 j0 b_if2 t0 b_if2_after_check2 b_if2_after_check1 j4 cmp_for dummyOperand1 jmp_for |
	
	"This is the running example in the ABCD paper, reproduced it just to have something to validate our results agains
	The code is kinda ugly, I don't know if there's a nicer way to build this self-referencial/cyclic IRs"
	
	"             /->------------------------------------------------------------------------------------->-\
	             / V--<-------------------<-\                                   /->---------------------/->--\
	 |b_i| -> |b_while| -> |b_if1| -> |b_for| -> |b_if2| -> |b_if2_after_check1| -> |b_if2_after_check2| -> |b_end|
                                           \-<------------------------------------------------------<-/
	"
	
	"Check Figure 3 in ABCD: Eliminating Array Bounds Checks on Demand for it to make *a bit* more sense"

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

	b_for := cfg newBasicBlock.

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
		
	b_while := cfg newBasicBlockWith: [ :block | |cmp|
		limit1 := block phiWith: {limit0. limit3}.
		st1 := block phiWith: {st0. st3}.
		cmp := block less: st1 than: limit1.
		block jumpIf: cmp to: b_if1 ifFalseTo: b_end
	].

	b_if1 instructions first replaceOperand: dummyOperand1 by: st1.
	b_if1 instructions second replaceOperand: dummyOperand1 by: limit1.

	b_i jumpTo: b_while.

	jmp_for falseBranch removePredecessor: jmp_for basicBlock.
	jmp_for newFalseBranch: b_while.
	jmp_for basicBlock predecessors add: jmp_for basicBlock predecessors removeFirst.

	
	cfg validate.
	self insertPiNodes: cfg.

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testInsertPiNodesEqualsCondition [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].

	self insertPiNodes: cfg.
	
	self assert: cfg b2 piNodes size equals: 2.
	self assert: (cfg b2 piNodes allSatisfy: [ :pi | pi constraint class == DREqualsObjectConstraint ]).
	
	self assert: cfg b3 piNodes size equals: 2.
	self assert: (cfg b3 piNodes allSatisfy: [ :pi | pi constraint class == DRNotEqualsObjectConstraint ]).

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testInsertPiNodesRemoval [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].

	self optimize: cfg with: {DRNoOptimisation}.
	
	self assert: cfg b2 piNodes size equals: 0.
	
	self assert: cfg b3 piNodes size equals: 0.

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testPiInsertionBreaksCriticalEdges [

	| cfg newBlock |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
			
	cfg b1 lastInstruction replaceTarget: cfg b2 by: cfg b4.
	cfg b4 addPredecessor: cfg b1.
	cfg b4 removePredecessor: cfg b2.	
	cfg b2 removeFromCFG.

	self insertPiNodes: cfg.
	
	self assert: cfg blocks size equals: 5.
	self deny: (cfg b1 successors includes: cfg b4).
	newBlock := cfg blockById: 5.
	self assert: (cfg b1 successors includes: newBlock).	
	self assert: (newBlock successors includes: cfg b4).	
	
	self assert: newBlock piNodes size equals: 2.
	self assert: cfg b4 piNodes size equals: 0.
]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testUsagesInDominatedBlocksAreReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
	
	self insertPiNodes: cfg.
	
	store := cfg b2 instructions third.
	self assert: store isStore.
	self assert: store operand1 isPiNode
]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testUsagesInNonDominatedBlocksAreNotReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
	
	self insertPiNodes: cfg.
	
	store := cfg b4 instructions first.
	self assert: store isStore.
	
	self deny: store operand1 isPiNode
]
