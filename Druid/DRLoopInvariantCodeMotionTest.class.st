Class {
	#name : #DRLoopInvariantCodeMotionTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

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

	self assert: (loopInvariantInstruction operand2) equals: (DRPhysicalRegister name: 'ReceiverResultReg')
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
	storeInstruction := loopBodyBlock store: 0 into: 0.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intructions were not moved"
	self assert: (loopBodyBlock includesInstruction: loadInstruction).
	self assert: (loopBodyBlock includesInstruction: storeInstruction).
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveLoopVariantInstructionOutsideOfLoop [

	| cfg loopBlock loopVariantInstruction |
	cfg := self generateDruidIRFor: #primitiveLoopWithLoopVariantInstruction.

	loopBlock := cfg blockById: 7.
	loopVariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intruction was not moved"
	self assert: (loopBlock includesInstruction: loopVariantInstruction)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesConstantInstructionOutsideNestedLoops [

	| cfg loopBlock loopInvariantInstruction preheaderBlock |
	cfg := self generateDruidIRFor: #primitiveNestedLoopWithInvariant.

	loopBlock := cfg blockById: 9.
	loopInvariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.
	
	preheaderBlock := cfg blockById: 12.

	self deny: (loopBlock includesInstruction: loopInvariantInstruction).
	self assert: (preheaderBlock includesInstruction: loopInvariantInstruction)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesConstantInstructionOutsideOfLoop [

	| cfg loopBlock loopInvariantInstruction preLoopBlock preheaderBlock |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	preLoopBlock := cfg blockById: 4.
	loopBlock := cfg blockById: 7.
	loopInvariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.
	
	self flag: #TODO.
	"Loop Invariant code should be hoisted outside both loops, if possible.
	To achieve that, we should optimize inner loops before the outer ones."
	preheaderBlock := cfg blockById: 9.

	self deny: (loopBlock includesInstruction: loopInvariantInstruction).
	self assert: (preheaderBlock includesInstruction: loopInvariantInstruction)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesMultipleRelatedInstructionsOutsideOfLoop [

	| cfg loopBlock preLoopBlock loopInvariantInstruction1 loopInvariantInstruction2 preheaderBlock1 preheaderBlock2 |
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	preLoopBlock := cfg blockById: 4.
	loopBlock := cfg blockById: 7.
	loopInvariantInstruction1 := loopBlock firstInstruction.
	loopInvariantInstruction2 := loopBlock instructions second.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	preheaderBlock1 := cfg blockById: 11.
	preheaderBlock2 := cfg blockById: 13.

	self deny: (loopBlock includesInstruction: loopInvariantInstruction1 ).
	self deny: (loopBlock includesInstruction: loopInvariantInstruction2 ).
	self assert: (preheaderBlock1 includesInstruction: loopInvariantInstruction1).
	self assert: (preheaderBlock2 includesInstruction: loopInvariantInstruction2)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionWrapsMovedInstructionWithIf [

	| cfg loopBlock preLoopBlock loopInvariantInstruction preheaderBlock |
	cfg := self generateDruidIRFor: #primitiveNoopLoopWithInvariant.

	preLoopBlock := cfg blockById: 4.
	loopBlock := cfg blockById: 7.
	loopInvariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.
	
	preheaderBlock := cfg blockById: 9.

	self assert: (preheaderBlock includesInstruction: loopInvariantInstruction)
]
