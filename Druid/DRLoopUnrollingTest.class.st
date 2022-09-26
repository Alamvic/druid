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
