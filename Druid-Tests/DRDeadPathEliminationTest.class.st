Class {
	#name : #DRDeadPathEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #initialization }
DRDeadPathEliminationTest >> previousOptimizations [

	^ { 
		  DRFailureCodeBasedTailDuplication new.
		  DRSCCP new.
		  DRCopyPropagation new.
		  DRDeadCodeElimination new.
		  DRBranchCollapse new.
		  (DRDeadBlockElimination then: DRCleanControlFlow).
		  DRPhiSimplication new. }
]

{ #category : #initialization }
DRDeadPathEliminationTest >> setUp [

	super setUp.
	self configureInterpreter.
	optimisation := DRDeadEdgeSplitting then: DRDeadBranchElimination
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDeadPathWithOnePredecessor [

	| cfg lastBlocks lastPaths newMergePoint oldMergePoint newBlock exitBasicBlock |
	cfg := self generateDruidIRFor: #primitiveSandclock.
	oldMergePoint := cfg mergeBlocks first.
	newMergePoint := oldMergePoint newEmptyPredecessor.
	exitBasicBlock := cfg exitBasicBlocks anyOne.
	
	newBlock := cfg newBasicBlockWith: [ :b | b copy: 1 ].
	oldMergePoint removePredecessorAt: 1.
	newMergePoint
		jumpIf: (DRPhysicalGeneralPurposeRegister name: 'Parameter')
		to: oldMergePoint
		ifFalseTo: newBlock.
	newBlock jumpTo: exitBasicBlock.

	2 timesRepeat: [ 
		cfg applyOptimisation: optimisation.
		cfg applyOptimisation: DRCleanControlFlow new ].

	cfg generatePaths.

	self assert: cfg deadPaths isEmpty.
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDeadPathsOnLoop [

	| cfg lastPaths conditionalConstraint |
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.
	cfg generatePaths.

	self assert: cfg deadPaths isNotEmpty.

	optimisation applyTo: cfg.
	self optimize: cfg.
	cfg generatePaths.

	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 2.

	"Should propagate conditional constraint to the result"
	conditionalConstraint := lastPaths first constraintFor: 'ssa_7'.
	self
		assertConstraint: (DRGreaterOrEqualsConstraint withValue: 15)
		includes: conditionalConstraint
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDoubleDeadPaths [
	"TOFIX: it's trying to create phis in wrong 'frontier' blocks"

	| cfg lastBlocks lastPaths lastPathsOne lastPathsTwo lastPathsThird |
	cfg := self generateDruidIRFor: #primitiveDNA.

	4 timesRepeat: [ 
		cfg applyOptimisation: optimisation.
		self optimize: cfg ].

	cfg generatePaths.

	lastPaths := cfg exitBasicBlocks flatCollect: [ :e | cfg incomingEdgesFor: e ].
	lastBlocks := cfg exitBasicBlocks.
	lastPathsOne := cfg incomingEdgesFor: lastBlocks first.
	lastPathsTwo := cfg incomingEdgesFor: lastBlocks second.
	lastPathsThird := cfg incomingEdgesFor: lastBlocks third.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 4. "X>10 || 10<=X>7 || 7<=X>0 || 0<=X"
	self assert: lastPathsOne size equals: 1.
	self assert: lastPathsTwo size equals: 1.
	self assert: lastPathsThird size equals: 2.
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDoubleHalfDeadPaths [

	| cfg lastBlocks lastPaths |
	cfg := self generateDruidIRFor: #primitiveDNA2.
	3 timesRepeat: [
		optimisation applyTo: cfg.
		2 timesRepeat: [self optimize: cfg ]	].
	
	cfg generatePaths.
	lastPaths := cfg exitBasicBlocks flatCollect: [:e | cfg incomingEdgesFor: e ].
	lastBlocks := cfg exitBasicBlocks.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 4.
	self assert: (cfg incomingEdgesFor: lastBlocks first) size equals: 1.
	self assert: (cfg incomingEdgesFor: lastBlocks second) size equals: 1.
	self assert: (cfg incomingEdgesFor: lastBlocks third) size equals: 1.
	self assert: (cfg incomingEdgesFor: lastBlocks fourth) size equals: 1.
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateInnerDeadPaths [

	| cfg lastBlocks |

	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	optimisation applyTo: cfg.
	self optimize: cfg.
	cfg generatePaths.

	lastBlocks := cfg exitBasicBlocks.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isCopy.
	self assert: lastBlocks first firstInstruction operand1 value equals: 99.
	self assert: lastBlocks second firstInstruction isCopy.
	self assert: lastBlocks second firstInstruction operand1 value equals: 17.

]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateOnlyDeadPaths [

	| cfg lastBlocks lastPaths lastPathsLeft lastPathsRight |
	cfg := self generateDruidIRFor: #primitiveMultiplePathsOnlyOneDead.

	optimisation applyTo: cfg.
	cfg generatePaths.

	lastPaths := cfg exitBasicBlocks flatCollect: [ :e | cfg incomingEdgesFor: e ].
	lastBlocks := cfg exitBasicBlocks.
	lastPathsLeft := cfg incomingEdgesFor: lastBlocks first.
	lastPathsRight := cfg incomingEdgesFor: lastBlocks last.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 5.
	self assert: lastPathsLeft size equals: 3.
	self assert: lastPathsRight size equals: 2
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateSimpleDeadPaths [

	| cfg lastBlocks lastPaths |
	cfg := self generateDruidIRFor: #primitiveSandclock.

	optimisation applyTo: cfg.
	self optimize: cfg.
	cfg generatePaths.

	lastBlocks := cfg exitBasicBlocks.
	lastPaths := cfg exitBasicBlocks flatCollect: [ :e | cfg incomingEdgesFor: e ].

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 3.

	self assert: lastBlocks second firstInstruction isPhiFunction. "One branch finishes in a phi"
	self assert: lastBlocks first firstInstruction isCopy.
	self
		assert: lastBlocks first firstInstruction operand1 value
		equals: 116 "Other branch harcodes the value"
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testNotSplitUnnecessaryBlocks [

	"TO CHECK if we want this behavior, this simple case finish with same instructions count but 1 block more"

	| cfg mergeBlock |
	cfg := self generateDruidIRFor: #primitiveMultiplePathsOnlyOneDead.

	mergeBlock := cfg mergeBlocks first.
	
	self assert: mergeBlock predecessors size equals: 3.

	optimisation applyTo: cfg.
	self optimize: cfg.

	"It doesn't duplicate to all predecessors, just dead paths"
	self assert: mergeBlock predecessors size equals: 2.

]
