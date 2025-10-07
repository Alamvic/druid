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
	
	self assert: cfg b4 phiFunctions size equals: 0.

]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testPhisInsertedInIfMergePoint [

	| cfg blocks b_i b_if_true b_if_false b_end phis |
	cfg := (DRIRParser for: '[[b0]]
b_i:
	i := Copy(0)
	j := Copy(1)
	_ := If(LessThan, i, j; b_if_true, b_if_false)

b_if_true:
	_ := Jump(b_end)

b_if_false:
	_ := Jump(b_end)

b_end:[b_if_true, b_if_false]
	_ := Store(8, i, 777)

b0:
	_ := Jump(b_i)
') parse.

	blocks := cfg blocks.
	b_i := blocks at: 1.
	b_if_true := blocks at: 2.
	b_if_false := blocks at: 3.
	b_end := blocks at: 4.

	self assert: b_end phiFunctions isEmpty.

	self insertPiNodes: cfg.

	self assert: b_end phiFunctions size equals: 2.

	phis := b_end phiFunctions.

	self assert: (phis allSatisfy: [ :phi | phi operands allSatisfy: #isPiNode ]).
	
	self assert: (phis anySatisfy: [ :phi | phi operands allSatisfy: [ :pi | pi simpleConstantFold = 0 ] ]).
	self assert: (phis anySatisfy: [ :phi | phi operands allSatisfy: [ :pi | pi simpleConstantFold = 1 ] ]).

	self assert: (b_end instructions anySatisfy: [:i | i isStore and: [i operands anySatisfy: [:op | op isPhiFunction and: [ op simpleConstantFold = 0 ] ]]])
	
]

{ #category : 'tests' }
DRPiNodesInsertionTest >> testPhisOfPhisInsertedInIfMergePoint [

	| cfg blocks phis block_C block_D block_E block_F |
	"case A->{B, C}, B->D, C->{D,E}, D->F, E->F}
	Phis should be inserted both in D and in F,
	and the F should have the one in D as one of its operands"
	
	
	cfg := self setUpCFGWithConditionalBetweenVars:
				[ :cfg :var1 :var2 | cfg instructionFactory equals: var1 to: var2 ].

	blocks := cfg blocks.
	block_C := blocks at: 3.
	block_D := blocks at: 4.
	block_E := cfg newBasicBlock.
	block_F := cfg newBasicBlock.
	block_D removePredecessor: block_C.
	block_C jumpIf: (cfg instructionFactory equals: 1 to: 2) to: block_D ifFalseTo: block_E.
	block_D jumpTo: block_F.
	block_E jumpTo: block_F.
	
	self insertPiNodes: cfg.

	self deny: block_D phiFunctions isEmpty.
	self deny: block_F phiFunctions isEmpty.

	phis := block_F phiFunctions.

	self assert: (phis allSatisfy: [ :phi | phi operands anySatisfy: #isPiNode ]).
	self assert: (phis allSatisfy: [ :phi | phi operands anySatisfy: #isPhiFunction ]).
	
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
DRPiNodesInsertionTest >> testUsagesAreReplacedInPhiFunctionIfPredecesorIsDominated [

	| cfg blocks b_error b_while2 b_while phi pi |
	
	cfg := (DRIRParser for: '[[b0]]
b_i:
	i0 := Copy(0)
	_ := Jump(b_while)
b_while:[b_i, b_while2]
	i1 := phi(i0, i2)
	_ := If(LessThan, i1, 3; b_while2, b_end)

b_while2:
	i2 := Add(i1, 1)
	_ := If(LessThan, i2, 5; b_while, b_end)

b_end:
	_ := Store(8, 2, 777)

b0:
	_ := Jump(b_i)
') parse.

	blocks := cfg blocks.
	b_error := blocks at: 5.
	b_while2 := blocks at: 3.
	b_while := blocks at: 2.


	self insertPiNodes: cfg.

	
	phi := b_while instructions first.
	self assert: phi isPhiFunction.
	self assert: phi operands first operand value equals: 0.
	
	pi := phi operands second.
	self assert: pi isPiNode.
	self assert: pi operand isAdd.
	self assert: pi constraint class equals: DRPiLessThanConstraint 



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
	
	store := cfg b4 instructions third.
	self assert: store isStore.
	
	self deny: store operand1 isPiNode
]
