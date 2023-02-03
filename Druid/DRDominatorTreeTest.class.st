Class {
	#name : #DRDominatorTreeTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierIterationWithBranch1 [
"
	  T1  →   T2 = e   →   T3   
          \_________/ 
"

	| cfg dominatorTree b1 b2 b3 |
	
	cfg := DRControlFlowGraph new.
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b3.

	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
	   
   self assert: (dominatorTree iteratedDominanceFrontierOf: b1) equals: { } asSet.
   self assert: (dominatorTree iteratedDominanceFrontierOf: b2) equals: { b3 } asSet.
   self assert: (dominatorTree iteratedDominanceFrontierOf: b3) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierIterationWithBranch2 [
	| cfg dominatorTree |
	cfg := self setUpCFGWithConditional.

	
   dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
	
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b1) equals: {  } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b2) equals: {cfg b4} asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b3) equals: {cfg b4} asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b4) equals: {  } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierIterationWithBranch3 [
"
       /------------\  
	 B5 →  B1 →   B2  → B4
             \→  B3 /
"

	| cfg dominatorTree b5 |
	cfg := self setUpCFGWithConditional.
	
	b5 := cfg initialBasicBlock.
	cfg initialBasicBlock jumpTo: b5.
	b5 jumpIfTrueTo: cfg b4 ifFalseTo: cfg b1.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
   
   self assert: (dominatorTree iteratedDominanceFrontierOf: b5) equals: {  } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b1) equals: { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b2) equals: { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b3) equals: { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b4) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierIterationWithBranch4 [
"
		 
	   /----------------\     
	 B6 →  B5 →  B1 →   B2  → B4
            \      \→  B3 /
             \________/
"

	| cfg dominatorTree b5 b6 |
	
	cfg := self setUpCFGWithConditional.
	
	b6 := cfg initialBasicBlock.
	b5 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b6.
	b6  jumpIfTrueTo: cfg b2 ifFalseTo: b5.
	b5 jumpIfTrueTo: cfg b1 ifFalseTo: cfg b3.

	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
   
   self assert: (dominatorTree iteratedDominanceFrontierOf: b6) equals: { } asSet.
   self assert: (dominatorTree iteratedDominanceFrontierOf: b5) equals: { cfg b2 . cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b1) equals: { cfg b2 . cfg b3 . cfg b4} asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b2) equals: { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b3) equals: { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b4) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierWithBranch1 [
"
	  T1  →   T2 = e   →   T3   
          \_________/ 
"

	| cfg dominatorTree b1 b2 b3 |
	
	cfg := DRControlFlowGraph new.
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b3.

	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
   
   self assert: (dominatorTree dominanceFrontierOf: b1) equals: { } asSet.
   self assert: (dominatorTree dominanceFrontierOf: b2) equals: { b3 } asSet.
   self assert: (dominatorTree dominanceFrontierOf: b1) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierWithBranch2 [
	| cfg dominatorTree |
	cfg := self setUpCFGWithConditional.

	
   dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
	
	self assert: (dominatorTree dominanceFrontierOf: cfg b2) equals: {cfg b4} asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b1) equals: {  } asSet.
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierWithBranch3 [
"
       /------------\  
	 B5 →  B1 →   B2  → B4
             \→  B3 /
"

	| cfg dominatorTree b5 |
	cfg := self setUpCFGWithConditional.
	
	b5 := cfg initialBasicBlock.
	cfg initialBasicBlock jumpTo: b5.
	b5 jumpIfTrueTo: cfg b4 ifFalseTo: cfg b1.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
   
   self assert: (dominatorTree dominanceFrontierOf: b5) equals: {  } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b1) equals: { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b2) equals: { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b3) equals: { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b4) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorFrontierWithBranch4 [
"
		 
	   /----------------\     
	 B6 →  B5 →  B1 →   B2  → B4
            \      \→  B3 /
             \________/
"

	| cfg dominatorTree b5 b6 |
	
	cfg := self setUpCFGWithConditional.
	
	b6 := cfg initialBasicBlock.
	b5 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b6.
	b6  jumpIfTrueTo: cfg b2 ifFalseTo: b5.
	b5 jumpIfTrueTo: cfg b1 ifFalseTo: cfg b3.

	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
   
   self assert: (dominatorTree dominanceFrontierOf: b6) equals: { } asSet.
   self assert: (dominatorTree dominanceFrontierOf: b5) equals: { cfg b2 . cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b1) equals: { cfg b2 . cfg b3 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b2) equals: { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b3) equals: { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b4) equals: { } asSet.
	
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeIterationWithBranch4 [
	| cfg b6 b5 dominatorTree |
	cfg := self setUpCFGWithConditional.
	
	b6 := cfg initialBasicBlock.
	b5 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b5. 
	b6 jumpIfTrueTo: cfg b2 ifFalseTo: b5.
	b5 jumpIfTrueTo: cfg b1 ifFalseTo: cfg b3.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
	
	self assert: (dominatorTree iteratedDominanceFrontierOf: b6) equals:  { } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: b5) equals:  { cfg b2 . cfg b4} asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b1) equals:  { cfg b2 . cfg b3 . cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b2) equals:  { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b3) equals:  { cfg b4 } asSet.
	self assert: (dominatorTree iteratedDominanceFrontierOf: cfg b4) equals:  { } asSet.
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithBranch4 [
	| cfg b6 b5 dominatorTree |
	cfg := self setUpCFGWithConditional.
	
	b6 := cfg initialBasicBlock.
	b5 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b5. 
	b6 jumpIfTrueTo: cfg b2 ifFalseTo: b5.
	b5 jumpIfTrueTo: cfg b1 ifFalseTo: cfg b3.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree buildDominatorTreeOf: cfg.
	
	self assert: (dominatorTree dominanceFrontierOf: b6) equals:  { } asSet.
	self assert: (dominatorTree dominanceFrontierOf: b5) equals:  { cfg b2 . cfg b4} asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b1) equals:  {cfg b2 . cfg b3 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b2) equals:  { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b3) equals:  { cfg b4 } asSet.
	self assert: (dominatorTree dominanceFrontierOf: cfg b4) equals:  { } asSet.
]

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTreeWithBranchAtDifferentDepth [
	| cfg b0jump |

	b0jump := DRBranchIfTrue operands: { DRConstantValue value: true }.
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
	
	jump := DRBranchIfTrue operands: { DRConstantValue value: true }.
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
