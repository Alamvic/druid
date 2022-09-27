Class {
	#name : #DRLoopUnrollingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #running }
DRLoopUnrollingTest >> optimize: cfg [

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new)
]

{ #category : #initialization }
DRLoopUnrollingTest >> setUp [

	super setUp.
	self configureInterpreter.
	compilerCompiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new.
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesBodyBlock [

	| unrollingFactor cfg loop |
	unrollingFactor := 2.
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	cfg applyOptimisation: (DRLoopUnrolling withUnrollingFactor: unrollingFactor).

	loop := cfg allLoops anyOne.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: unrollingFactor
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesBodyBlockMultipleTimes [

	| unrollingFactor cfg loop |
	unrollingFactor := 4.
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	cfg applyOptimisation: (DRLoopUnrolling withUnrollingFactor: unrollingFactor).

	loop := cfg allLoops anyOne.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: unrollingFactor
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesMultipleBodyBlocks [

	| unrollingFactor cfg loop loopBlock |
	unrollingFactor := 2.
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	loopBlock := cfg blockById: 7.
	loopBlock second breakBasicBlock.

	cfg applyOptimisation: (DRLoopUnrolling withUnrollingFactor: unrollingFactor).

	loop := cfg allLoops anyOne.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: 2 * unrollingFactor
]
