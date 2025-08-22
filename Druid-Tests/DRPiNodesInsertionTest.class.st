Class {
	#name : 'DRPiNodesInsertionTest',
	#superclass : 'DRPiNodesTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'tests' }
DRPiNodesInsertionTest >> testInsertPiNodesEqualsCondition [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].

	self insertPiNodes: cfg.
	
	self assert: cfg b2 piNodes size equals: 2.
	self assert: (cfg b2 piNodes allSatisfy: [ :pi | pi constraint class == DRPiEqualsConstraint ]).
	
	self assert: cfg b3 piNodes size equals: 2.
	self assert: (cfg b3 piNodes allSatisfy: [ :pi | pi constraint class == DRPiNotEqualsConstraint ]).

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testInsertPiNodesRemoval [

	| cfg |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].

	self optimize: cfg with: {DRNoOptimisation}.
	
	self assert: cfg b2 piNodes size equals: 0.
	
	self assert: cfg b3 piNodes size equals: 0.

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testPiInsertionBreaksCriticalEdges [

	| cfg newBlock |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
			
	cfg b1 lastInstruction replaceTarget: cfg b2 by: cfg b4.
	cfg b4 addPredecessor: cfg b1.
	cfg b4 removePredecessor: cfg b2.	
	cfg b2 removeFromCFG.

	self insertPiNodes: cfg.
	
	self assert: cfg blocks size equals: 5.
	self deny: (cfg b1 successors includes: cfg b4).
	newBlock := cfg blockById: 5.
	self assert: (cfg b1 successors includes: newBlock).	
	self assert: (newBlock successors includes: cfg b4).	
	
	self assert: newBlock piNodes size equals: 2.
	self assert: cfg b4 piNodes size equals: 0.
]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testUsagesInDominatedBlocksAreReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
	
	self insertPiNodes: cfg.
	
	store := cfg b2 instructions third.
	self assert: store isStore.
	self assert: store operand1 isPiNode
]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testUsagesInNonDominatedBlocksAreNotReplaced [

	| cfg store |
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].
	
	self insertPiNodes: cfg.
	
	store := cfg b4 instructions first.
	self assert: store isStore.
	
	self deny: store operand1 isPiNode
]
