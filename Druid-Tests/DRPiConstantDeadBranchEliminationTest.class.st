Class {
	#name : 'DRPiConstantDeadBranchEliminationTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiConstantDeadBranchEliminationTest >> testDetectRedundantIfConstantWithConstant [

	"
	                 /->-|b_end0|
	 |b_i| -> |b_if1| -> |b_end1| (unreachable)
	"

	| cfg b_end0 b_end1 b_i x b_if1 |
	cfg := DRControlFlowGraph new.
	
	b_end0 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 777 ].
	b_end1 := cfg newBasicBlockWith: [ :block | block storeSInt64: 2 at: 888 ].
	
	b_i := cfg newBasicBlockWith: [ :block |
		x := block copy: 4.
	].

	b_if1 := cfg newBasicBlockWith: [ :block | |cmp|
		cmp := block less: x than: 3.
		block jumpIf: cmp to: b_end1 ifFalseTo: b_end0
	].

	cfg initialBasicBlock jumpTo: b_i.
	b_i jumpTo: b_if1.

	self optimize: cfg with: { DRPiConstantDeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end1) not.
	self assert: b_if1 endInstruction isJump.
	self assert: b_if1 endInstruction isConditionalBranch not.
	self assert: b_if1 endInstruction target equals: b_end0.

]

{ #category : 'tests' }
DRPiConstantDeadBranchEliminationTest >> testDetectRedundantIfVarWithConstants [

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

	self optimize: cfg with: { DRPiConstantDeadBranchElimination }.
	
	self assert: (cfg blocks includes: b_end2) not.
	self assert: b_if2 endInstruction isJump.
	self assert: b_if2 endInstruction isConditionalBranch not.
	self assert: b_if2 endInstruction target equals: b_end1.

]

{ #category : 'tests' }
DRPiConstantDeadBranchEliminationTest >> testKillingOneBranchInBlockWithManyPredecessors [
	"                
	                           /--->-|b_end1|
	 |b0| -> |b_if1| -> |b_if2| -X-> |b_end2|
	               \ -> |b_jmp| -->-/
	"

	| cfg b_end1 b_end2 b_if2 blocks b_if1 b_jmp b_to_merge1 b_to_merge2 pi phi |
	cfg := (DRIRParser for: '[[b0]]
b_if1:
	x := LoadFramePointer()
	_ := If(LessThan, x, 5; b_if2, b_jmp)
b_if2:
	_ := If(LessThan, x, 5; b_end1, b_end2)

b_jmp:
	_ := Jump(b_end2)
b_end1:
	_ := Store(8, 2, 888)
b_end2:
	_ := Store(8, 2, 999)

b0:
	_ := Jump(b_if1)
') parse.

	blocks := cfg blocks.
	b_if1 := blocks at: 1.
	b_if2 := blocks at: 2.
	b_jmp := blocks at: 3.
	b_end1 := blocks at: 4.
	b_end2 := blocks at: 5.
	
	self insertPiNodes: cfg.
	
	"re-add critical edges (broken during pi-insertion)"
	b_to_merge1 := b_if1 successors detect: [ :b | b~= b_if2 ].
	b_to_merge2 := b_if2 successors detect: [ :b | b~= b_end1 ].

	b_if1 replaceSuccessor: b_to_merge1 by: b_end2.
	b_if2 replaceSuccessor: b_to_merge2 by: b_end2.
	b_end2 replacePredecessor: b_to_merge1 by: b_if1.
	b_end2 replacePredecessor: b_to_merge2 by: b_if2.

	phi := b_end2 phiFunctions unique.
	phi := phi replaceOperandsIfDominated: phi operand1 withAll: { b_if2 instructions first. b_if1 instructions first }.
	phi := phi replaceOperandsIfDominated: phi operand2 withAll: { b_if2 instructions first. b_if1 instructions first }.
	
	pi := DRPiNode constraint: (DRPiGreaterOrEqualsConstraint withValue: 5 asDRValue) on: phi.
	pi result: (cfg instructionFactory allocateTemporaryRegister).
	b_end2 addInstructionFirst: pi.
	
	b_to_merge1 removeFromCFG.
	b_to_merge2 removeFromCFG.

	cfg validate.
	
	
	
	"the actual optimization"
	cfg applyOptimisation: DRPiConstantDeadBranchElimination.
	
	

	self assert: (cfg blocks includesAll: {b_if1. b_if2}).
	self assert: b_end2 predecessors size equals: 1.
	self deny: b_end2 predecessors unique equals: b_if2.
	
	self assert: b_if2 endInstruction isJump.
	self assert: b_if2 endInstruction isConditionalBranch not.
	self assert: b_if2 endInstruction target equals: b_end1

]
