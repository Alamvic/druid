Class {
	#name : #DRDeadPathEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRDeadPathEliminationTest >> previousOptimizations [

	^ { 
		  DRPhiSimplication new.
		  (DRDeadBlockElimination then: DRCleanControlFlow).
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

{ #category : #queries }
DRDeadPathEliminationTest >> testDeadPathsInference [

	| cfg deadPath expectedTarget pathBlockIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	deadPath := cfg deadPaths associations first.
	pathBlockIds := deadPath value collect: [ :e | e destination id ].
	expectedTarget := self anyIncomingEdge: cfg for: (cfg blockById: 6).

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: pathBlockIds hasSameElements: { 7. 18. 1 }
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDoubleDeadPaths [
	"TOFIX: it's trying to create phis in wrong 'frontier' blocks"

	| cfg lastBlocks lastPaths lastPathsLeft lastPathsRight |
	cfg := self generateDruidIRFor: #primitiveDNA.

	3 timesRepeat: [ 
		cfg applyOptimization: optimisation.
		self optimize: cfg ].

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

	3 timesRepeat: [ 
		optimisation applyTo: cfg.
		self optimize: cfg ].

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

{ #category : #queries }
DRDeadPathEliminationTest >> testManyDeadPaths [

	| cfg deadPaths deadTargetIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranches.

	deadPaths := cfg deadPaths.
	deadTargetIds := deadPaths keys collect: [ :e | e destination id ].

	self assert: deadPaths size equals: 2.
	self assertCollection: deadTargetIds hasSameElements: { 10. 6 }
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testNotSplitUnnecessaryBlocks [

	"TO CHECK if we want this behavior, this simple case finish with same instructions count but 1 block more"

	| cfg |
	cfg := self generateDruidIRFor: #primitiveMultiplePathsOnlyOneDead.

	optimisation applyTo: cfg.
	self optimize: cfg.

	self assert: cfg instructions size equals: 21.
	self assert: cfg blocks size equals: 9
]
