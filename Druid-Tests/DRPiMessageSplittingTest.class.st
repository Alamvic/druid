Class {
	#name : 'DRPiMessageSplittingTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiMessageSplittingTest >> testTwiceSameConstantConditionWithSecondIfAtMergePoint [

	| cfg blocks b_if1_false b_if1_true b_if1 b_merge1 b_merge2 |
	cfg := (DRIRParser for: '[[b0]]
b_if1:
	_ := If(LessThan, 4, x; b_if1_true, b_if1_false)

b_if1_true:
	_ := Jump(b_merge)

b_if1_false:
	_ := Jump(b_merge)

b_merge:
	_ := If(LessThan, 4, x; b_end1, b_end2)

b_end1:
	_ := Store(8, 2, 777)

b_end2:
	_ := Store(8, 2, 888)

b0:
	x := LoadFramePointer()
	_ := Jump(b_if1)
') parse.

	blocks := cfg blocks.
	b_if1 := blocks at: 1.
	
	self
		assert: b_if1 successors first lastInstruction target
		equals: b_if1 successors second lastInstruction target.

	self optimize: cfg with: { DRPiZ3MessageSplitting }.

	self
		assert: b_if1 successors first lastInstruction targets size
		equals: 2.
	self
		assert: b_if1 successors first lastInstruction targets
		equals: b_if1 successors second lastInstruction targets.

]
