Class {
	#name : #DRControlFlowGraphTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRControlFlowGraphTest >> testIterateChainOfBlocksIteratesAllBlocks [

	| block1 block2 cfg |
	block1 := DRBasicBlock new.
	block2 := DRBasicBlock new.
	
	block1 addInstruction: (DRAdd operands: {1. 2}).
	block1 jumpTo: block2.
	block2 addInstruction: (DRAdd operands: {2. 3}).
	
	cfg := DRControlFlowGraph new
		initialBasicBlock: block1;
		exitBasicBlock: block2;
		yourself.

	self
		assert: cfg instructions
		equals: block1 instructions , block2 instructions
]

{ #category : #tests }
DRControlFlowGraphTest >> testIterateConditionalCFGIteratesAllBlocks [

	| block1 blockTrue blockFalse joinBlock cfg conditionalBranch |
	block1 := DRBasicBlock new.
	blockTrue := DRBasicBlock new.
	blockFalse := DRBasicBlock new.
	joinBlock := DRBasicBlock new.
	
	block1 addInstruction: (conditionalBranch := DREqualsComparison operands: { 2.3 }).
	conditionalBranch trueBranch: blockTrue.
	conditionalBranch falseBranch: blockFalse.
	
	blockTrue addInstruction: (DRAdd operands: {2. 3}).
	blockTrue jumpTo: joinBlock.
	
	blockFalse addInstruction: (DRAdd operands: {3. 4}).
	blockFalse jumpTo: joinBlock.
	
	joinBlock addInstruction: (DRAdd operands: {4. 5}).
	
	cfg := DRControlFlowGraph new
		initialBasicBlock: block1;
		exitBasicBlock: joinBlock;
		yourself.

	self
		assert: cfg instructions
		equals: block1 instructions,
			blockTrue instructions,
			blockFalse instructions,
			joinBlock instructions
]

{ #category : #tests }
DRControlFlowGraphTest >> testIterateSingleEmptyBlockCFG [

	| block cfg |
	block := DRBasicBlock new.
	cfg := DRControlFlowGraph new
		initialBasicBlock: block;
		exitBasicBlock: block;
		yourself.

	cfg instructionsDo: [ :e | self fail ].
]
