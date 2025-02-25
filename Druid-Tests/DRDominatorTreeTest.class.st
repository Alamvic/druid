Class {
	#name : 'DRDominatorTreeTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testConditionalCFGDomination [

	| cfg b0 b1 b2 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b2.

	self assert: (b0 isDominatorOfExactly: {
				 b1.
				 b2 }).
	self assert: b1 dominatedBlocks isEmpty.
	self assert: b2 dominatedBlocks isEmpty
]

{ #category : 'old' }
DRDominatorTreeTest >> testCorrectDominatorTreeWithLoop [

	| cfg jump |
	jump := self newConditionalJump.
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
	self assert: (cfg b3 isDominatorOf: {  })
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testDegenerateLoopCFGDomination [

	| cfg b0 b1 b2 b3 b4 b5 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b4.
	b3 jumpIf: true to: b2 ifFalseTo: b5.
	b4 jumpIf: true to: b1 ifFalseTo: b5.

	self assert: (b0 isDominatorOfExactly: {
				 b1.
				 b2.
				 b3.
				 b4.
				 b5 }).
	self assert: (b1 isDominatorOfExactly: { b3 }).
	self assert: (b2 isDominatorOfExactly: { b4 }).
	self assert: b3 dominatedBlocks isEmpty.
	self assert: b4 dominatedBlocks isEmpty.
	self assert: b5 dominatedBlocks isEmpty
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testDegenerateLoopCFGImmediateDomination [

	| cfg b0 b1 b2 b3 b4 b5 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b4.
	b3 jumpIf: true to: b2 ifFalseTo: b5.
	b4 jumpIf: true to: b1 ifFalseTo: b5.

	self assert: (b0 isImmediateDominatorOfExactly: {
				 b1.
				 b2.
				 b5 }).
	self assert: (b1 isImmediateDominatorOfExactly: { b3 }).

	self assert: (b2 isImmediateDominatorOfExactly: { b4 }).
	self assert: b3 dominatedBlocks isEmpty.
	self assert: b4 dominatedBlocks isEmpty.
	self assert: b5 dominatedBlocks isEmpty
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testDiamondConditionalCFGDomination [

	| cfg b0 b1 b2 b3 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	
	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b3.

	self assert: (b0 isDominatorOfExactly: {
				 b1.
				 b2.
				 b3 }).
	self assert: b1 dominatedBlocks isEmpty.
	self assert: b2 dominatedBlocks isEmpty.
	self assert: b3 dominatedBlocks isEmpty
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testDoubleDiamondConditionalCFGDomination [

	| cfg b0 b1 b2 b3 b4 b5 b6 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b3.

	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.

	b3 jumpIf: true to: b4 ifFalseTo: b5.
	b4 jumpTo: b6.
	b5 jumpTo: b6.

	self assert: (b0 isDominatorOfExactly: {
				 b1.
				 b2.
				 b3.
				 b4.
				 b5.
				 b6 }).
	self assert: b1 dominatedBlocks isEmpty.
	self assert: b2 dominatedBlocks isEmpty.
	self assert: (b3 isDominatorOfExactly: {
				 b4.
				 b5.
				 b6 }).
	self assert: b4 dominatedBlocks isEmpty.
	self assert: b5 dominatedBlocks isEmpty.
	self assert: b6 dominatedBlocks isEmpty
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testDoubleDiamondConditionalCFGImmediateDomination [

	| cfg b0 b1 b2 b3 b4 b5 b6 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b3.

	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.

	b3 jumpIf: true to: b4 ifFalseTo: b5.
	b4 jumpTo: b6.
	b5 jumpTo: b6.

	self assert: (b0 isImmediateDominatorOfExactly: {
				 b1.
				 b2.
				 b3 })
]

{ #category : 'test - api' }
DRDominatorTreeTest >> testExactDominationFailsWithIncompleteNumberOfBlocks [

	| cfg b0 b1 b2 b3 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b3.
	b2 jumpTo: b3.

	"b0 dominates all, exactly checks all blocks"
	self assert: (b0 isDominatorOf: {
				 b1.
				 b2 }).
	self deny: (b0 isDominatorOfExactly: {
				 b1.
				 b2 })
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testLinearCFGDomination [

	| cfg b0 b1 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b0 jumpTo: b1.

	self assert: (b0 isDominatorOfExactly: { b1 }).
	self assert: (b1 isDominatedBy: b0)
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testNestedDiamondConditionalCFGDomination [

	| cfg b0 b1 b2 b3 b4 b5 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b5.
	b2 jumpIf: true to: b3 ifFalseTo: b4.
	b3 jumpTo: b5.
	b4 jumpTo: b5.

	self assert: (b0 isDominatorOfExactly: {
				 b1.
				 b2.
				 b3.
				 b4.
				 b5 }).
	self assert: b1 dominatedBlocks isEmpty.
	self assert: (b2 isDominatorOfExactly: {
				 b3.
				 b4 }).
	self assert: b3 dominatedBlocks isEmpty.
	self assert: b4 dominatedBlocks isEmpty.
	self assert: b5 dominatedBlocks isEmpty
]

{ #category : 'test - scenarios' }
DRDominatorTreeTest >> testNestedDiamondConditionalCFGImmediateDomination [

	| cfg b0 b1 b2 b3 b4 b5 |
	cfg := DRControlFlowGraphForTesting new.

	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	b5 := cfg newBasicBlock.

	b0 jumpIf: true to: b1 ifFalseTo: b2.
	b1 jumpTo: b5.
	b2 jumpIf: true to: b3 ifFalseTo: b4.
	b3 jumpTo: b5.
	b4 jumpTo: b5.

	self assert: (b0 isImmediateDominatorOfExactly: {
				 b1.
				 b2.
				 b5 }).
	self assert: b1 dominatedBlocks isEmpty.
	self assert: (b2 isDominatorOfExactly: {
				 b3.
				 b4 }).
	self assert: b3 dominatedBlocks isEmpty.
	self assert: b4 dominatedBlocks isEmpty.
	self assert: b5 dominatedBlocks isEmpty
]
