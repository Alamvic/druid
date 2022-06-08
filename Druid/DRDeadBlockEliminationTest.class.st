Class {
	#name : #DRDeadBlockEliminationTest,
	#superclass : #DROptimisationTest,
	#instVars : [
		'instructionFactory',
		'cfg',
		'deadBlock',
		'deadBlock2',
		'exitBlock',
		'exitPhi',
		'deadPhiOperand',
		'alivePhiOperand',
		'entryBlock'
	],
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRDeadBlockEliminationTest >> setUp [
	super setUp.
	
	instructionFactory := DRInstructionFactory new.
	optimisation := DRDeadBlockElimination new.
	
	cfg := DRControlFlowGraph new.
	entryBlock := cfg newBasicBlock.
	exitBlock := cfg newBasicBlock.
	deadBlock := cfg newBasicBlock.
	deadBlock2 := cfg newBasicBlock.

	deadBlock jumpTo: deadBlock2.

	deadBlock2 addInstruction: (deadPhiOperand := instructionFactory copy: 2).
	deadBlock2 jumpTo: exitBlock.
	
	entryBlock addInstruction: (alivePhiOperand := instructionFactory copy: 2).
	entryBlock jumpTo: exitBlock.
	
	exitBlock addInstruction: (exitPhi := instructionFactory phiWith: deadPhiOperand with: alivePhiOperand).

	cfg initialBasicBlock jumpTo: entryBlock.
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testKeepExitBlock [

	optimisation applyTo: cfg.
	
	self assert: (cfg blocks includes: exitBlock)
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testKeepInitialBlock [

	optimisation applyTo: cfg.
	
	self assert: (cfg blocks includes: entryBlock)
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testRemoveDeadBlock [

	optimisation applyTo: cfg.
	
	self deny: (cfg blocks includes: deadBlock)
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testRemoveDeadBlockBacklink [

	optimisation applyTo: cfg.
	
	self assert: (deadBlock successors noneSatisfy: [ :s | s hasPredecessor: deadBlock ])
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testRemoveDeadBlockChain [

	optimisation applyTo: cfg.
	
	self deny: (cfg blocks includes: deadBlock2)
]

{ #category : #tests }
DRDeadBlockEliminationTest >> testRemoveDeadBlockPhi [

	optimisation applyTo: cfg.
	
	self assert: exitPhi operands size equals: 1.
	self assert: exitPhi operands first equals: alivePhiOperand
]
