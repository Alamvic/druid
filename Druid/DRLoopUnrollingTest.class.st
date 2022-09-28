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
DRLoopUnrollingTest >> testLoopUnrollingAddsRolledLoopAfterUnrolledLoopToHandleRemainingElements [

	| unrollingFactor cfg loop rolledLoop rolledLoopInstructions |
	unrollingFactor := 2.
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	loop := cfg allLoops anyOne.
	rolledLoopInstructions := loop bodyInstructions.

	loop unroll: unrollingFactor.
	
	self assert: cfg allLoops size equals: 2.
	rolledLoop := cfg allLoops first.
	rolledLoop bodyInstructions with: rolledLoopInstructions do: [ :copy :original |
		self assert: copy opcode equals: original opcode ]
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesBodyBlock [

	| unrollingFactor cfg loop |
	unrollingFactor := 2.
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	loop := cfg allLoops anyOne.

	loop unroll: unrollingFactor.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: unrollingFactor
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesBodyBlockMultipleTimes [

	| unrollingFactor cfg loop |
	unrollingFactor := 4.
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	loop := cfg allLoops anyOne.

	loop unroll: unrollingFactor.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: unrollingFactor
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesMultipleBodyBlocks [

	| unrollingFactor cfg loop loopBlock |
	unrollingFactor := 2.
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	loopBlock := cfg blockById: 7.
	loopBlock second breakBasicBlock.

	loop := cfg allLoops anyOne.
	
	loop unroll: unrollingFactor.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: 2 * unrollingFactor
]

{ #category : #tests }
DRLoopUnrollingTest >> testLoopUnrollingDuplicatesMultipleBodyBlocksMultipleTimes [

	| unrollingFactor cfg loop loopBlock |
	unrollingFactor := 4.
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	loopBlock := cfg blockById: 7.
	loopBlock second breakBasicBlock.

	loop := cfg allLoops anyOne.

	loop unroll: unrollingFactor.

	self assert: (loop bodyInstructions count: [ :i | i isAdd ]) equals: 2 * unrollingFactor
]
