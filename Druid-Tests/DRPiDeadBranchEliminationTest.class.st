Class {
	#name : 'DRPiDeadBranchEliminationTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiDeadBranchEliminationTest >> testDetectRedundantIfWithConstants [

	"                 /->--------->-|b_end0|
	                 /          /->-|b_end1|
	 |b_i| -> |b_if1| -> |b_if2| -> |b_end2| (unreachable)
	"

	| cfg b_end0 b_end1 b_end2 b_i x b_if2 b_if1 |
	cfg := DRControlFlowGraph new.
	
	b_end0 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 777 ].
	b_end1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	b_end2 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 999 ].
	
	b_i := cfg newBasicBlockWith: [ :block |
		x := block loadFramePointer.
	].

	b_if2 := cfg newBasicBlockWith: [ :block | |cmp|
		cmp := block less: x than: 5.
		block jumpIf: cmp to: b_end1 ifFalseTo: b_end2
	].

	b_if1 := cfg newBasicBlockWith: [ :block | |cmp|
		cmp := block less: x than: 3.
		block jumpIf: cmp to: b_if2 ifFalseTo: b_end0
	].

	cfg initialBasicBlock jumpTo: b_i.
	b_i jumpTo: b_if1.

	self optimize: cfg with: { DRPiDeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end2) not.
	self assert: b_if2 endInstruction isJump.
	self assert: b_if2 endInstruction isConditionalBranch not.
	self assert: b_if2 endInstruction target equals: b_end1.

]
