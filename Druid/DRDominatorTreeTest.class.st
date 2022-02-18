Class {
	#name : #DRDominatorTreeTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTree [
	| cfg b0 b1 b2 b3 b4 jump dominatorTree |
	cfg := DRControlFlowGraphForTesting new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b0.
	b0 jumpTo: b1.
	jump := DRConditionalJump operands: { DRConstantValue value: true  }.
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	b2 addPredecessor: b1. 
	jump falseBranch: b3.
	b3 addPredecessor: b1.
	b3 jumpTo: b4.
	b2 jumpTo: b4.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	self assert: dominatorTree tree children size == 1.
]
