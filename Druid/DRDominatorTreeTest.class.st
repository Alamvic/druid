Class {
	#name : #DRDominatorTreeTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithBranchAtDifferentDepth [
	| cfg b1jump b0jump dominatorTree |
	b1jump := DRConditionalJump operands: { DRConstantValue value: true }.
	b0jump := DRConditionalJump operands: { DRConstantValue value: true }.
	cfg := self setUpCFGWithConditional: b1jump.
	b0jump trueBranch: cfg b4.
	b0jump falseBranch: cfg b1.
	cfg b4 addPredecessor: cfg b0.
	cfg b0 endInstruction: b0jump.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	self assert: dominatorTree tree children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b0) children size == 2.
	self assert: (dominatorTree blockDictionary at: cfg b1) children size == 2.
	self assert: (dominatorTree blockDictionary at: cfg b2) children size == 0.
	self assert: (dominatorTree blockDictionary at: cfg b3) children size == 0.
	self assert: (dominatorTree blockDictionary at: cfg b4) children size == 0.
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithLoop [
	| cfg jump dominatorTree |
	jump := DRConditionalJump operands: { DRConstantValue value: true }.
	cfg := self setUpCFG: 3.
	jump trueBranch: cfg b3.
	jump falseBranch: cfg b1.
	cfg b1 addPredecessor: cfg b2.
	cfg b2 endInstruction: jump.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	self assert: dominatorTree tree children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b0) children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b1) children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b2) children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b3) children size == 0.
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithSimpleBranch [
	| cfg jump dominatorTree |
	jump := DRConditionalJump operands: { DRConstantValue value: true }.
	cfg := self setUpCFGWithConditional: jump.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	self assert: dominatorTree tree children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b0) children size == 1.
	self assert: (dominatorTree blockDictionary at: cfg b1) children size == 3.
	self assert: (dominatorTree blockDictionary at: cfg b2) children size == 0.
	self assert: (dominatorTree blockDictionary at: cfg b3) children size == 0.
	self assert: (dominatorTree blockDictionary at: cfg b4) children size == 0.
]
