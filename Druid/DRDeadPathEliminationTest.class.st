Class {
	#name : #DRDeadPathEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRDeadPathEliminationTest >> previousOptimizations [

	^ { 
		  DRBranchCollapse new.
		  (DRDeadBlockElimination then: DRCleanControlFlow).
		  DRPhiSimplication new.
		  DRFailureCodeBasedTailDuplication new.
		  DRSCCP new.
		  DRCopyPropagation new.
		  DRDeadCodeElimination new }
]

{ #category : #initialization }
DRDeadPathEliminationTest >> setUp [

	super setUp.
	self configureInterpreter.
	optimisation := DRDeadPathSplitting then: DRDeadPathElimination
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDeadPathWithOnePredecessor [

	| cfg lastBlocks lastPaths newMergePoint oldMergePoint newBlock |
	cfg := self generateDruidIRFor: #primitiveSandclock.
	oldMergePoint := cfg blockById: 31.
	newMergePoint := oldMergePoint newEmptyPredecessor.
	newBlock := cfg newBasicBlockWith: [ :b | b copy: 1 ].
	oldMergePoint removePredecessorAt: 1.
	newMergePoint
		jumpIf: (DRPhysicalRegister name: 'Parameter')
		to: oldMergePoint
		ifFalseTo: newBlock.
	newBlock jumpTo: (cfg blockById: 1).
	cfg generatePaths.

	2 timesRepeat: [ 
		cfg applyOptimisation: optimisation.
		cfg applyOptimisation: DRCleanControlFlow new ].

	lastBlocks := cfg lastBasicBlock predecessors.
	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.

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

	"Should propagate conditional exit"
	conditionalConstraint := lastPaths first constraintFor: 'R10'.
	self
		assert: (DRGreaterOrEqualsConstraint withValue: 15)
		includes: conditionalConstraint
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDoubleDeadPaths [
	"TOFIX: it's trying to create phis in wrong 'frontier' blocks"

	| cfg lastBlocks lastPaths lastPathsLeft lastPathsRight |
	cfg := self generateDruidIRFor: #primitiveDNA.

	3 timesRepeat: [ 
		cfg applyOptimisation: optimisation.
		self optimize: cfg ].

	cfg generatePaths.

	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.
	lastBlocks := cfg lastBasicBlock predecessors.
	lastPathsLeft := cfg incomingEdgesFor: lastBlocks first.
	lastPathsRight := cfg incomingEdgesFor: lastBlocks last.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 4. "X>10 || 10<=X>7 || 7<=X>0 || 0<=X"
	self assert: lastPathsLeft size equals: 2.
	self assert: lastPathsRight size equals: 2
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDoubleHalfDeadPaths [

	| cfg lastBlocks lastPaths lastPathsLeft lastPathsRight |
	cfg := self generateDruidIRFor: #primitiveDNA2.
	cfg generatePaths.
	3 timesRepeat: [
		optimisation applyTo: cfg.
		self optimize: cfg ].
	
	cfg generatePaths.
	
	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.
	lastBlocks := cfg lastBasicBlock predecessors.
	lastPathsLeft := cfg incomingEdgesFor: lastBlocks first.
	lastPathsRight := cfg incomingEdgesFor: lastBlocks last.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 4.
	self assert: lastPathsLeft size equals: 3.
	self assert: lastPathsRight size equals: 1
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateInnerDeadPaths [

	| cfg lastBlocks |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	optimisation applyTo: cfg.
	self optimize: cfg.

	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isCopy.
	self assert: lastBlocks first firstInstruction operand1 value equals: 17.
	self assert: lastBlocks second firstInstruction isCopy.
	self assert: lastBlocks second firstInstruction operand1 value equals: 99.
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateOnlyDeadPaths [

	| cfg lastBlocks lastPaths lastPathsLeft lastPathsRight |
	cfg := self generateDruidIRFor: #primitiveMultiplePathsOnlyOneDead.

	optimisation applyTo: cfg.

	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.
	lastBlocks := cfg lastBasicBlock predecessors.
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
	self optimize: cfg .

	lastBlocks := cfg lastBasicBlock predecessors.
	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 3.

	self assert: lastBlocks first firstInstruction isPhiFunction. "One branch finishes in a phi"
	self assert: lastBlocks second firstInstruction isCopy.
	self
		assert: lastBlocks second firstInstruction operand1 value
		equals: 116 "Other branch harcodes the value"
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testNotSplitUnnecessaryBlocks [

	"TO CHECK if we want this behavior, this simple case finish with same instructions count but 1 block more"

	| cfg |
	cfg := self generateDruidIRFor: #primitiveMultiplePathsOnlyOneDead.

	optimisation applyTo: cfg.
	self optimize: cfg.

	self assert: cfg instructions size equals: 18.
	self assert: cfg blocks size equals: 9
]
