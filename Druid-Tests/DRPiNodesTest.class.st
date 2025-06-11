Class {
	#name : 'DRPiNodesTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'running' }
DRPiNodesTest >> insertPiNodes: cfg [
	cfg applyOptimisation: DRBranchCollapse new.
	
	"DRBranchCollapse messes up the backjumps
	TODO - Should every optimization leave backjumps in a consistent state, just in case? If so, we should validate that in `cfg validate`
			 Or should DRInsertPiNodes call `cfg fixBackJumps` when applied?"
	cfg fixBackJumps.
	
	cfg applyOptimisation: DRInsertPiNodes new.

]

{ #category : 'running' }
DRPiNodesTest >> optimize: cfg with: optimisations [

	self insertPiNodes: cfg.
	cfg applyOptimisation: (optimisations reduce: [ :o1 :o2 | o1 then: o2 ]).
	cfg applyOptimisation: DRCopyPropagation new.
	cfg applyOptimisation: DRDeadCodeElimination new.
]

{ #category : 'running' }
DRPiNodesTest >> setUpCFGWithConditionalBetweenVars: aBlock [

	| var1 var2 cfg instructionFactory realCond |
	
	cfg := self setUpCFGWithConditional.
	
	instructionFactory := cfg instructionFactory.
	var1 := cfg b1 addInstruction: instructionFactory loadFramePointer.
	var2 := cfg b1 addInstruction: instructionFactory loadFramePointer.
	realCond := (aBlock value: cfg value: var1 value: var2).
	cfg b1 replaceInstruction: (cfg b1 instructions at: cfg b1 instructions size - 1) by: realCond.
	cfg b1 endInstruction operands: {cfg b1 endInstruction condition. realCond. true}.
	
	cfg b2 storeSInt64: var1 at: 777.
	cfg b3 storeSInt64: var1 at: 888.
	cfg b4 storeSInt64: var2 at: 999.

	cfg validate.

	^ cfg.
	
]
