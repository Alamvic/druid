Class {
	#name : #DRCleanControlFlowTest,
	#superclass : #TestCase,
	#instVars : [
		'instructionFactory',
		'optimisation'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRCleanControlFlowTest >> setUp [
	super setUp.
	
	instructionFactory := DRInstructionFactory new.
	optimisation := DRCleanControlFlow new
]

{ #category : #'tests - redundant branches' }
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

{ #category : #'tests - redundant branches' }
DRCleanControlFlowTest >> testFoldRedundantBranch [

	| b0 b1 jump cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	jump trueBranch: b1.
	jump falseBranch: b1.

	optimisation foldRedundantBranch: b0 endInstruction.
	
	self assert: b0 endInstruction isNonConditionalJump.
	self assert: b0 endInstruction target equals: b1.
]

{ #category : #'tests - redundant branches' }
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

{ #category : #'tests - redundant branches' }
DRCleanControlFlowTest >> testRemoveEmptyBlock [

	| b0 b1 b2 cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	
	b0 jumpTo: b1.
	b1 jumpTo: b2.

	optimisation removeEmptyBlock: b1.
	
	self assert: b0 endInstruction target equals: b2
]

{ #category : #'tests - redundant branches' }
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

	optimisation removeEmptyBlock: emptyBlock.
	
	self assert: b1 endInstruction target equals: b4.
	self assert: b2 endInstruction target equals: b4.
]

{ #category : #'tests - redundant branches' }
DRCleanControlFlowTest >> testShouldFoldRedundantBranch [

	| b0 b1 jump cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg newBasicBlock.
	b1 := cfg newBasicBlock.
	
	b0 endInstruction: (jump := instructionFactory jumpIf: true asDRValue).
	jump trueBranch: b1.
	jump falseBranch: b1.

	self assert: (optimisation shouldFoldRedundantBranch: b0 endInstruction)
]

{ #category : #'tests - redundant branches' }
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

{ #category : #'tests - redundant branches' }
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

{ #category : #'tests - redundant branches' }
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
