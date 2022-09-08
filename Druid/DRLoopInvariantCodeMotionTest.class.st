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
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveLoadsNorStores [

	| cfg initialBlock loopConditionBlock loopBodyBlock loopExitBlock loopCondition loadInstruction storeInstruction |
	cfg := DRControlFlowGraphForTesting new.

	initialBlock := cfg initialBasicBlock.
	loopCondition := DRConstantValue value: true.

	loopConditionBlock := cfg newBasicBlock.
	loopBodyBlock := cfg newBasicBlock.
	loopExitBlock := cfg newBasicBlock.

	initialBlock jumpTo: loopConditionBlock.
	loopConditionBlock jumpIf: loopCondition to: loopBodyBlock ifFalseTo: loopExitBlock.

	loopBodyBlock backJumpTo: loopConditionBlock.

	loadInstruction := loopBodyBlock load: 0.
	storeInstruction := loopBodyBlock store: 0 into: 0.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intructions were not moved"
	self assert: (loopBodyBlock containsInstruction: loadInstruction).
	self assert: (loopBodyBlock containsInstruction: storeInstruction).
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionDoesNotMoveLoopVariantInstructionOutsideOfLoop [

	| cfg loopBlock loopVariantInstruction |
	cfg := self generateDruidIRFor: #primitiveLoopWithLoopVariantInstruction.

	loopBlock := cfg blockById: 7.
	loopVariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.

	"Intruction was not moved"
	self assert: (loopBlock containsInstruction: loopVariantInstruction)
]

{ #category : #tests }
DRLoopInvariantCodeMotionTest >> testLoopInvariantCodeMotionMovesConstantInstructionOutsideOfLoop [

	| cfg loopBlock loopInvariantInstruction preLoopBlock preheaderBlock |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	preLoopBlock := cfg blockById: 4.
	loopBlock := cfg blockById: 7.
	loopInvariantInstruction := loopBlock firstInstruction.

	cfg applyOptimisation: DRLoopInvariantCodeMotion new.
	
	preheaderBlock := cfg blockById: 9.

	self deny: (loopBlock containsInstruction: loopInvariantInstruction).
	self assert: (preheaderBlock containsInstruction: loopInvariantInstruction)
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

	self deny: (loopBlock containsInstruction: loopInvariantInstruction1 ).
	self deny: (loopBlock containsInstruction: loopInvariantInstruction2 ).
	self assert: (preheaderBlock1 containsInstruction: loopInvariantInstruction1).
	self assert: (preheaderBlock2 containsInstruction: loopInvariantInstruction2)
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

	self assert: (preheaderBlock containsInstruction: loopInvariantInstruction)
]
