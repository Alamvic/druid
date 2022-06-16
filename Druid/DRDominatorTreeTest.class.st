Class {
	#name : #DRDominatorTreeTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithBranchAtDifferentDepth [
	| cfg b0jump |
1halt.
	b0jump := DRBranchIfCondition operands: { DRConstantValue value: true }.
	cfg := self setUpCFGWithConditional.
	b0jump trueBranch: cfg b4.
	b0jump falseBranch: cfg b1.
	cfg b4 addPredecessor: cfg b0.
	cfg b0 endInstruction: b0jump.
	
	cfg initialBasicBlock controlFlowGraph: cfg.
	
	self assert: (cfg b0 isDominatorOf: { cfg b1 . cfg b4 }).
	self assert: (cfg b1 isDominatorOf: { cfg b2 . cfg b3 }).
	self assert: (cfg b2 isDominatorOf: { }).
	self assert: (cfg b3 isDominatorOf: { }).
	self assert: (cfg b4 isDominatorOf: { }).
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithLoop [
	| cfg jump |
1halt.
	jump := DRBranchIfCondition operands: { DRConstantValue value: true }.
	cfg := self setUpCFG: 3.
	jump trueBranch: cfg b3.
	jump falseBranch: cfg b1.
	cfg b1 addPredecessor: cfg b2.
	cfg b2 endInstruction: jump.
	
	cfg initialBasicBlock controlFlowGraph: cfg.
	cfg b0 controlFlowGraph: cfg.
	cfg b1 controlFlowGraph: cfg.
	cfg b2 controlFlowGraph: cfg.
	cfg b3 controlFlowGraph: cfg.
	
	self assert: (cfg b0 isDominatorOf: { cfg b1 }).
	self assert: (cfg b1 isDominatorOf: { cfg b2 }).
	self assert: (cfg b2 isDominatorOf: { cfg b3 }).
	self assert: (cfg b3 isDominatorOf: { }).
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithSimpleBranch [
	| cfg |
	cfg := self setUpCFGWithConditional.
	
	cfg initialBasicBlock controlFlowGraph: cfg.
	
	self assert: (cfg b0 isDominatorOf: { cfg b1 }).
	self assert: (cfg b1 isDominatorOf: { cfg b2 . cfg b3 . cfg b4 }).
	self assert: (cfg b2 isDominatorOf: { }).
	self assert: (cfg b3 isDominatorOf: { }).
	self assert: (cfg b4 isDominatorOf: { }).
]
