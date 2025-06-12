Class {
	#name : 'DRPiABCDDeadBranchEliminationTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiABCDDeadBranchEliminationTest >> testABCDGraph [
	| cfg b_i b_end limit0 limit1 st0 st1 b_while b_for j1 b_if1 st3 limit3 j0 b_if2 t0 b_if2_after_check2 b_if2_after_check1 j4 cmp_for dummyOperand1 jmp_for b_check_failed |
	
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
	b_check_failed := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 777 ].
		
	b_i := cfg newBasicBlockWith: [ :block |
		dummyOperand1 := block copy: 1234.
		limit0 := block loadFramePointer.
		st0 := block copy: -1.
	].
	cfg initialBasicBlock jumpTo: b_i.
	
	b_if2_after_check2 := cfg newBasicBlockWith: [ :block |
		j4 := block add: 1 to: dummyOperand1 "j1".
	].

	b_if2_after_check1 := cfg newBasicBlockWith: [ :block | |check|
		t0 := block add: 1 to: dummyOperand1 "j1".
		check := block less: t0 than: limit0.
		block jumpIf: check to: b_if2_after_check2 ifFalseTo: b_check_failed.
	].
	b_if2 := cfg newBasicBlockWith: [ :block | |check|
		check := block less: dummyOperand1 "j1" than: limit0.
		block jumpIf: check to: b_if2_after_check1 ifFalseTo: b_check_failed.
	].

	b_for := cfg newBasicBlock.

	b_if2_after_check2 jumpTo: b_for.

	b_if1 := cfg newBasicBlockWith: [ :block |
		st3 := block add: 1 to: dummyOperand1 "st1".
		limit3 := block subtract: 1 from: dummyOperand1 "limit1".
		j0 := block copy: st3.
		block jumpTo: b_for.
	].

	cmp_for := b_for less: dummyOperand1 than: limit3.
	jmp_for := b_for jumpIf: cmp_for to: b_if2 ifFalseTo: b_i.

	j1 := b_for phiWithVariables: {1234 asDRValue . 4567 asDRValue }.
	cmp_for replaceOperand: dummyOperand1 by: j1.
	
	b_if2_after_check2 instructions first replaceOperand: dummyOperand1 by: j1.
	b_if2_after_check1 instructions first replaceOperand: dummyOperand1 by: j1.
	b_if2 instructions first replaceOperand: dummyOperand1 by: j1.

	j1 replaceOperand: j1 operands first by: j0.
	j1 replaceOperand: j1 operands second by: j4.
		
	b_while := cfg newBasicBlockWith: [ :block | |cmp|
		limit1 := block phiWithVariables: {}.
		st1 := block phiWithVariables: {}.
		cmp := block less: st1 than: limit1.
		block jumpIf: cmp to: b_if1 ifFalseTo: b_end
	].

	b_if1 instructions first replaceOperand: dummyOperand1 by: st1.
	b_if1 instructions second replaceOperand: dummyOperand1 by: limit1.

	b_i jumpTo: b_while.

	jmp_for falseBranch removePredecessor: jmp_for basicBlock.
	jmp_for newFalseBranch: b_while.
	jmp_for basicBlock predecessors add: jmp_for basicBlock predecessors removeFirst.
	
	limit1 operands: {limit0. limit3}.
	st1 operands: {st0. st3}.

	cfg validate.


	self optimize: cfg with: { DRPiABCDDeadBranchElimination }.


	self assert: b_if2 successors size equals: 1.
	self assert: b_if2 successors unique ~= b_check_failed.
	
	self assert: b_if2_after_check1 successors size equals: 1.
	self assert: b_if2 successors unique ~= b_check_failed.
	
	self assert: (cfg blocks includes: b_check_failed) not.

]

{ #category : 'tests' }
DRPiABCDDeadBranchEliminationTest >> testABCDWithAdditionOverSameVariable [

	"
	                 /->-|b_end0| (unreachable)
	 |b_i| -> |b_if1| -> |b_end1|
	"

	| cfg b_end0 b_end1 b_i x y b_if1 |
	cfg := DRControlFlowGraph new.
	
	b_end0 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 777 ].
	b_end1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	
	b_i := cfg newBasicBlockWith: [ :block |
		x := block loadSInt64: 111.
		y := block add: 1 to: x.
	].

	b_if1 := cfg newBasicBlockWith: [ :block | |cmp|
		cmp := block less: y than: x.
		block jumpIf: cmp to: b_end0 ifFalseTo: b_end1
	].

	cfg initialBasicBlock jumpTo: b_i.
	b_i jumpTo: b_if1.


	self optimize: cfg with: { DRPiABCDDeadBranchElimination }.

	self skip. "Pretty sure this needs lower bounds check algorithm to work"
	self deny: (cfg blocks includes: b_end0).
	self assert: (cfg blocks includes: b_end1).

	self assert: b_if1 endInstruction isJump.
	self assert: b_if1 endInstruction isConditionalBranch not.
	self assert: b_if1 endInstruction target equals: b_end1.

]

{ #category : 'tests' }
DRPiABCDDeadBranchEliminationTest >> testABCDWithSubstractionOverSameVariable [

	"
	                 /->-|b_end0|
	 |b_i| -> |b_if1| -> |b_end1| (unreachable)
	"

	| cfg b_end0 b_end1 b_i x y b_if1 |
	cfg := DRControlFlowGraph new.
	
	b_end0 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 777 ].
	b_end1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	
	b_i := cfg newBasicBlockWith: [ :block |
		x := block loadSInt64: 111.
		y := block subtract: 1 from: x.
	].

	b_if1 := cfg newBasicBlockWith: [ :block | |cmp|
		cmp := block less: y than: x.
		block jumpIf: cmp to: b_end0 ifFalseTo: b_end1
	].

	cfg initialBasicBlock jumpTo: b_i.
	b_i jumpTo: b_if1.


	self optimize: cfg with: { DRPiABCDDeadBranchElimination }.

	
	self assert: (cfg blocks includes: b_end0).
	self deny: (cfg blocks includes: b_end1).

	self assert: b_if1 endInstruction isJump.
	self assert: b_if1 endInstruction isConditionalBranch not.
	self assert: b_if1 endInstruction target equals: b_end0.

]
