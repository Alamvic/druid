Class {
	#name : 'DRCleanControlFlowTest',
	#superclass : 'DROptimisationTest',
	#instVars : [
		'instructionFactory'
	],
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'running' }
DRCleanControlFlowTest >> setUp [
	super setUp.
	
	instructionFactory := DRInstructionFactory new.
	optimisation := DRCleanControlFlow new
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testCannotCombineBlocksWhenSecondBlockContainsPhiFunctions [

	| b0 b1 b2 copy cfg |
	cfg := self setUpCFG: 3.
	b0 := cfg b1.
	b1 := cfg b2.
	b2 := cfg b3.

	copy := b1 copy: 2.	
	b2 phiWith: copy.

	self deny: (optimisation shouldCombineBlock: b1)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testCombineBlocks [

	| b0 b1 b2 copy add combinedBlock cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.

	b0 jumpTo: b1.
	b1 addInstruction: (copy := instructionFactory copy: 2).	
	b1 jumpTo: b2.
	b2 addInstruction: (add := instructionFactory add: 2 to: 3).

	optimisation combineBlocks: b1 and: b2 fromCfg: cfg.
	
	combinedBlock := b0 endInstruction target.
	self assert: combinedBlock instructions first equals: copy.
	self assert: combinedBlock instructions second equals: add
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testDoNotRemoveEmptyBlockIfItCreatesPredecessorSuccessorAmbiguities [

	| cfg c1 c2 |
	cfg := self setUpCFGWithConditional.
	
	c1 := cfg b1 copy: 1.
	c2 := cfg b1 copy: 2.
	
	"If there is no phi in the successors, they can be merged."

	cfg applyOptimisation: DRCleanControlFlow new.

	self assert: cfg b1 successors asSet size equals: 1
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testFoldRedundantBranch [

	| b0 b1 jump cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	jump trueBranch: b1.
	jump falseBranch: b1.
	b1 addPredecessor: b0.

	optimisation foldRedundantBranch: b0 endInstruction.
	
	self assert: b0 endInstruction isNonConditionalJump.
	self assert: b0 endInstruction target equals: b1.
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testFoldRedundantBranchMergesPredecessors [

	| b0 b1 jump cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	
	jump trueBranch: b1.
	b1 addPredecessor: b0.
	
	jump falseBranch: b1.
	b1 addPredecessor: b0.

	optimisation foldRedundantBranch: b0 endInstruction.
	
	"Before we had two predecessors, now we have 1"
	self assert: b0 successors first predecessors size equals: 1
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testHoistBranch [

	| b0 b1 b2 b3 b4 branch cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	b0 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	branch trueBranch: b1.
	branch falseBranch: b2.
	b1 jumpTo: b2.
	b2 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	
	branch trueBranch: b3.
	b3 addPredecessor: b2.
	
	branch falseBranch: b4.
	b4 addPredecessor: b2.

	optimisation hoistBranchInto: b1.
	
	self assert: b1 endInstruction equals: branch
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testHoistBranchWithMultiplePredecessors [

	| b0 b1 b2 b3 b4 branch cfg b11 |
	
	cfg := self setUpCFGWithConditional.
	b0 := cfg b1.
	b1 := cfg b2.
	b11 := cfg b3.
	b2 := cfg b4.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	b2 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	
	branch trueBranch: b3.
	b3 addPredecessor: b2.
	
	branch falseBranch: b4.
	b4 addPredecessor: b2.

	optimisation hoistBranchInto: b1.
	
	"We have hoisted B2's branch into B1, but B2 should still be valid"
	b2 validate.
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testRemoveEmptyBlock [

	| b0 b1 b2 cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	
	b0 jumpTo: b1.
	b1 jumpTo: b2.

	optimisation removeEmptyBlock: b1 fromCfg: cfg.
	
	self assert: b0 endInstruction target equals: b2
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testRemoveEmptyBlockDoesNotReorderPhiFunctionFirstPredecessor [

	| b0 b1 b2 cfg r0 r1 b3 |
	cfg := self setUpCFGWithConditional.
	b0 := cfg b1.
	r0 := b0 copy: 0.
	
	b1 := cfg b2.
	r1 := b1 copy: 1.
	
	"Empty, to remove"
	b2 := cfg b3.
	
	"Joint point"
	b3 := cfg b4.
	b3 phiWith: r1 with: r0.

	optimisation removeEmptyBlock: b2 fromCfg: cfg.
	
	b3 validatePhiFunctions.
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testRemoveEmptyBlockDoesNotReorderPhiFunctionSecondPredecessor [

	| cfg r0 r1 phi |
	cfg := self setUpCFGWithConditional.
	r0 := cfg b1 copy: 0.
	r1 := cfg b2 copy: 1.
	"b3 empty"
	phi := cfg b4 phiWith: r1 with: r0.
	
	self assertCollection: phi operands hasSameElements: {r0 . r1}.

	optimisation removeEmptyBlock: cfg b3 fromCfg: cfg.
	
	cfg b4 validatePhiFunctions.
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testRemoveEmptyBlockWithManyPredecessors [

	| b0 b1 b2 emptyBlock b4 cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	emptyBlock := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	
	b0 endInstruction: (instructionFactory jumpIf: true asDRValue).
	b0 endInstruction trueBranch: b1.
	b0 endInstruction falseBranch: b2.
	b1 jumpTo: emptyBlock.
	b2 jumpTo: emptyBlock.
	emptyBlock jumpTo: b4.

	optimisation removeEmptyBlock: emptyBlock fromCfg: cfg.
	
	self assert: b1 endInstruction target equals: b4.
	self assert: b2 endInstruction target equals: b4.
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldFoldRedundantBranch [

	| b0 b1 jump cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	jump trueBranch: b1.
	jump falseBranch: b1.

	self assert: (optimisation shouldFoldRedundantBranchIn: b0)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldFoldRedundantBranchWithSameTargetAndSamePhiValues [

	| b0 b1 jump cfg r0 |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	r0 := b0 copy: 1.
	
	cfg initialBasicBlock jumpTo: b0.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	b1 addPredecessor: b0.
	b1 addPredecessor: b0.

	jump trueBranch: b1.
	jump falseBranch: b1.
	
	b1 phiWith: r0 with: r0.

	self assert: (optimisation shouldFoldRedundantBranch: b0 endInstruction)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldHoistBranch [

	| b0 b1 b2 b3 b4 branch cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	b0 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	branch trueBranch: b1.
	branch falseBranch: b2.
	b1 jumpTo: b2.
	b2 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	
	branch trueBranch: b3.
	b3 addPredecessor: b2.
	
	branch falseBranch: b4.
	b4 addPredecessor: b2.

	self assert: (optimisation shouldHoistBranchInto: b1)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldNotFoldRedundantBranch [

	| b0 b1 jump b2 cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	jump trueBranch: b1.
	jump falseBranch: b2.

	self deny: (optimisation shouldFoldRedundantBranch: b0 endInstruction)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldNotFoldRedundantBranchWithSameTargetButDifferentPhiValues [

	| b0 b1 jump cfg r0 r1 |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	r0 := b0 copy: 1.
	r1 := b0 copy: 2.
	
	cfg initialBasicBlock jumpTo: b0.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	b1 addPredecessor: b0.
	b1 addPredecessor: b0.
	
	
	jump trueBranch: b1.
	jump falseBranch: b1.
	
	b1 phiWith: r0 with: r1.

	self deny: (optimisation shouldFoldRedundantBranch: b0 endInstruction)
]

{ #category : 'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldNotHoistBranchBecauseNotEmpty [

	| b0 b1 b2 b3 b4 branch cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	b0 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	branch trueBranch: b1.
	branch falseBranch: b2.
	b1 jumpTo: b2.
	
	b2 addInstruction: (instructionFactory copy: 1).
	b2 endInstruction: (branch := instructionFactory jumpIf: true asDRValue).
	
	branch trueBranch: b3.
	b3 addPredecessor: b2.
	
	branch falseBranch: b4.
	b4 addPredecessor: b2.

	self deny: (optimisation shouldHoistBranchInto: b1)
]
