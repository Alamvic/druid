Class {
	#name : #DRDeadPathEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateDeadPathWithOnePredecessor [

	| cfg lastBlocks lastPaths newMergePoint oldMergePoint newBlock |
	cfg := self generateDruidIRFor: #primitiveSandclock.
	oldMergePoint := cfg blockById: 25.
	newMergePoint := oldMergePoint newEmptyPredecessor.
	newBlock := cfg newBasicBlockWith: [ :b | b copy: 1 ].
	oldMergePoint removePredecessorAt: 1.
	newMergePoint
		jumpIf: (DRPhysicalGeneralPurposeRegister name: 'Parameter')
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
	conditionalConstraint := lastPaths first constraintFor: 'R11'.
	self
		assertConstraint: (DRGreaterOrEqualsConstraint withValue: 15)
		includes: conditionalConstraint
]

{ #category : #optimization }
DRDeadPathEliminationTest >> testEliminateInnerDeadPaths [

	| cfg lastBlocks |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	optimisation applyTo: cfg.
	self optimize: cfg.
	cfg generatePaths.

	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isCopy.
	self assert: lastBlocks first firstInstruction operand1 value equals: 17.
	self assert: lastBlocks second firstInstruction isCopy.
	self assert: lastBlocks second firstInstruction operand1 value equals: 99.
]
