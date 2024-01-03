Class {
	#name : #DRLoopInvariantCodeMotionTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #assertion }
DRLoopInvariantCodeMotionTest >> assertLoop: aDRJump includes: aDRInstruction [

	self assert: (aDRJump loop instructions includes: aDRInstruction)
]

{ #category : #assertion }
DRLoopInvariantCodeMotionTest >> denyLoop: aDRJump includes: aDRInstruction [

	self deny: (aDRJump loop instructions includes: aDRInstruction)
]

{ #category : #running }
DRLoopInvariantCodeMotionTest >> optimize: cfg [

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionAdjustsPhiFunctionsOfMovedInstruction [

	| cfg loopBlock loopInvariantInstruction |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	loopBlock := cfg blockById: 7.
	loopInvariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self assert: loopInvariantInstruction operand2 isLoadReceiver
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveInstructionsInsideIf [

	| cfg ifBlock loopVariantInstruction |
	cfg := self generateDruidIRFor: #primitiveLoopWithLoopInvariantInstructionInsideIf.

	ifBlock := cfg blockById: 8.
	loopVariantInstruction := ifBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intruction was not moved"
	self assert: (ifBlock includesInstruction: loopVariantInstruction)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveLoadsNorStores [

	| cfg initialBlock loopConditionBlock loopBodyBlock loopExitBlock loopCondition loadInstruction storeInstruction |
	cfg := DRControlFlowGraphForTesting new.

	initialBlock := cfg initialBasicBlock.
	loopCondition := DRConstantValue value: true.

	loopConditionBlock := cfg newBasicBlock.
	loopBodyBlock := cfg newBasicBlock.
	loopExitBlock := cfg newBasicBlock.

	initialBlock jumpTo: loopConditionBlock.
	
	loopConditionBlock loopIf: loopCondition to: loopBodyBlock ifFalseTo: loopExitBlock.

	loadInstruction := loopBodyBlock load: 0.
	storeInstruction := loopBodyBlock storeSInt64: 0 at: 0.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intructions were not moved"
	self assert: (loopBodyBlock includesInstruction: loadInstruction).
	self assert: (loopBodyBlock includesInstruction: storeInstruction).
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveLoopVariantInstructionsOutsideOfLoop [

	| cfg loopBlock loopVariantInstructions |
	cfg := self generateDruidIRFor: #primitiveLoopWithLoopVariantInstructions.

	loopBlock := cfg blockById: 7.
	loopVariantInstructions := loopBlock instructions allButLast.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intruction was not moved"
	self assert: (loopVariantInstructions allSatisfy: [:i | loopBlock includesInstruction: i])
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesConstantInstructionOutsideNestedLoops [

	| cfg invariant |
	cfg := self generateDruidIRFor: #primitiveNestedLoopWithInvariant.

	invariant := cfg instructions detect: [ :i | i isMultiply ].

	self assertLoop: cfg allBackJumps first includes: invariant.
	self assertLoop: cfg allBackJumps second includes: invariant.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self assertLoop: cfg allBackJumps second includes: invariant.
	self denyLoop: cfg allBackJumps first includes: invariant.

]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesConstantInstructionOutsideOfLoop [

	| cfg invariant |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	invariant := cfg instructions detect: [ :i | i isMultiply ].

	self assertLoop: cfg allBackJumps first includes: invariant.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self denyLoop: cfg allBackJumps first includes: invariant.

]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesMultipleRelatedInstructionsOutsideOfLoop [

	| cfg invariant1 invariant2 |
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	invariant1 := cfg instructions detect: [ :i | i isMultiply ].
	invariant2 := invariant1 nextInstruction.

	self assertLoop: cfg allBackJumps first includes: invariant1.
	self assertLoop: cfg allBackJumps first includes: invariant2.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self denyLoop: cfg allBackJumps first includes: invariant1.
	self denyLoop: cfg allBackJumps first includes: invariant2
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionWorksWithLoopsWithMultipleBodyBlocks [

	| cfg  loopBlocks invariant1 invariant2 |
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	invariant1 := cfg instructions detect: [ :i | i isMultiply ].
	invariant2 := invariant1 nextInstruction.
	loopBlocks := invariant2 breakBasicBlock.
	
	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self denyLoop: cfg allBackJumps first includes: invariant1.
	self denyLoop: cfg allBackJumps first includes: invariant2
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionWrapsMovedInstructionWithIf [

	| cfg invariant |
	cfg := self generateDruidIRFor: #primitiveNoopLoopWithInvariant.

	invariant := cfg instructions detect: [ :i | i isCopy and: [ i operand1 value = 5 ] ].

	self assertLoop: cfg allBackJumps first includes: invariant.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	self denyLoop: cfg allBackJumps first includes: invariant.

]
