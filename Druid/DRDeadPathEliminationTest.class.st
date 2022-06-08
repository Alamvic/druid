Class {
	#name : #DRDeadPathEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRDeadPathEliminationTest >> previousOptimizations [

	^ { 
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
	oldMergePoint := cfg blockById: 30.
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

{ #category : #queries }
DRDeadPathEliminationTest >> testInferenceDeadPaths [

	| cfg deadPath expectedTarget pathBlockIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	cfg generatePaths.
	deadPath := cfg deadPaths associations first.
	pathBlockIds := deadPath value collect: [ :e | e destination id ].
	expectedTarget := self anyIncomingEdge: cfg for: (cfg blockById: 30).

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: pathBlockIds hasSameElements: { 15. 37. 1 }
]

{ #category : #queries }
DRDeadPathEliminationTest >> testInferenceManyDeadPaths [

	| cfg deadPaths deadTargetIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranches.

	cfg generatePaths.
	deadPaths := cfg deadPaths.
	deadTargetIds := deadPaths keys collect: [ :e | e destination id ].

	self assert: deadPaths size equals: 2.
	self assertCollection: deadTargetIds hasSameElements: { 43. 41 }
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

{ #category : #building }
DRDeadPathEliminationTest >> testUnmergeableConditionalBranchToSameTarget [

	| cfg b0 copy1 copy2 b1 condition |
	
	"If a block has a conditional branch to a single same target, it can only be merged and transformed to an unconditional jump if there is no Phi function depending on the branching.
	In this case, we should build two different paths."
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b0.
	
	copy1 := b0 copy: 1.
	copy2 := b0 copy: 2.
	condition := b0 greater: (DRPhysicalRegister name: 'NO SABES LOCO') than: 2.
	b1 := cfg newBasicBlock.
	b0 jumpIf: condition to: b1 ifFalseTo: b1.
	b1 phiWith: copy1 with: copy2.
	
	"Kill the condition boolean value, we don't care about it"
	"condition result: DRNoRegister new."
	cfg generatePaths.


	self assert: cfg deadPaths isEmpty
]
