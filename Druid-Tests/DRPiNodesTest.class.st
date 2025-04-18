Class {
	#name : 'DRPiNodesTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'running' }
DRPiNodesTest >> insertPiNodes: cfg [
	cfg applyOptimisation: DRCogitCanonicaliser new. 
	cfg applyOptimisation: DRInsertPiNodes new.

]

{ #category : 'running' }
DRPiNodesTest >> optimize: cfg with: optimisations [

	self insertPiNodes: cfg.
	
	cfg applyOptimisation: ((optimisations collect: [ :o | o new ]) reduce: [ :o1 :o2 | o1 then: o2 ]).
	
	cfg applyOptimisation: DRCopyPropagation new.
	cfg applyOptimisation: DRDeadCodeElimination new.
]

{ #category : 'running' }
DRPiNodesTest >> piNodesIn: aDRBasicBlock [

	^ aDRBasicBlock instructions select: [ :i | i isPiNode ]
]

{ #category : 'running' }
DRPiNodesTest >> setUpCFGWithConditionalBetweenVars: aBlock [

	| var1 var2 cfg instructionFactory cond jumpIf realCond |
	
	instructionFactory := DRInstructionFactory new.
	cond := instructionFactory equals: 1 to: 2.
	jumpIf := instructionFactory jumpIf: cond.
	cfg := self setUpCFGWithConditional: jumpIf.
	
	instructionFactory := cfg instructionFactory.
	var1 := instructionFactory loadFramePointer.
	var2 := instructionFactory loadFramePointer.

	realCond := (aBlock value: cfg value: var1 value: var2).
	cfg b1 replaceInstruction: cond by: realCond.
	cfg b1 replaceInstruction: jumpIf by: ((instructionFactory jumpIf: realCond)
		trueBranch: cfg b2;
		falseBranch: cfg b3;
		yourself).
		
	cfg b0 addInstructionFirst: var1.
	cfg b0 addInstructionFirst: var2.
	
	cfg b2 storeSInt64: var1 at: 777.
	cfg b3 storeSInt64: var1 at: 888.
	cfg b4 storeSInt64: var2 at: 999.

	cfg validate.

	^ cfg.
	
]

{ #category : 'tests' }
DRPiNodesTest >> testInsertPiNodesEqualsCondition [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].

	self insertPiNodes: cfg.
	
	self assert: (self piNodesIn: cfg b2) size equals: 2.
	self assert: ((self piNodesIn: cfg b2) allSatisfy: [ :pi | pi constraint class == DREqualsObjectConstraint ]).
	
	self assert: (self piNodesIn: cfg b3) size equals: 2.
	self assert: ((self piNodesIn: cfg b3) allSatisfy: [ :pi | pi constraint class == DRNotEqualsObjectConstraint ]).

]

{ #category : 'tests' }
DRPiNodesTest >> testInsertPiNodesRemoval [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].

	self optimize: cfg with: {DRNoOptimisation}.
	
	self assert: (self piNodesIn: cfg b2) size equals: 0.
	
	self assert: (self piNodesIn: cfg b3) size equals: 0.

]

{ #category : 'tests' }
DRPiNodesTest >> testPiInsertionBreaksCriticalEdges [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
			
	cfg b1 lastInstruction replaceTarget: cfg b2 by: cfg b4.
	cfg b4 addPredecessor: cfg b1.
	
	cfg b2 instructionsDo: [ :i | i removeFromCFG ].
	cfg removeBasicBlock: cfg b2.
	
	self insertPiNodes: cfg.
	
	self assert: cfg blocks size equals: 5.
	self assert: (cfg b1 successors includes: cfg b4) not.
	self assert: (cfg b1 successors includes: (cfg blocks detect: [ :b | b id == 5 ])).	
	
	self assert: ((cfg blocks detect: [ :b | b id == 5 ]) instructions anySatisfy: [ :i | i isPiNode ]).
	self assert: (cfg b4 instructions noneSatisfy: [ :i | i isPiNode ]).

]

{ #category : 'tests' }
DRPiNodesTest >> testUsagesInDominatedBlocksAreReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
	
	self insertPiNodes: cfg.
	
	store := cfg b2 instructions at: 3.
	self assert: store isStore.
	self assert: ((self piNodesIn: cfg b2) anySatisfy: [ :pi| pi result == store operand1]).

]

{ #category : 'tests' }
DRPiNodesTest >> testUsagesInNonDominatedBlocksAreNotReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var :cte | cfg instructionFactory equals: var to: cte ].
	
	self insertPiNodes: cfg.
	
	store := cfg b4 instructions first.
	self assert: store isStore.
	
	self assert: ((self piNodesIn: cfg b2) noneSatisfy: [ :pi| pi result == store operand1]).
	self assert: ((self piNodesIn: cfg b3) noneSatisfy: [ :pi| pi result == store operand1]).
	
]
