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
DRLoopUnrollingTest >> testLoopUnrollingReplicatesLoopBodyByItsUnrollingFactor [

	| unrollingFactor cfg loop loopInstruction |
	unrollingFactor := 4.
	cfg := self generateDruidIRFor: #primitiveLoopWithKnownIterations"#primitiveLoopIncrementing".

	loop := cfg allLoops anyOne.
	loopInstruction := loop bodyInstructions first.
1halt.
	cfg applyOptimisation: (DRLoopUnrolling withUnrollingFactor: unrollingFactor).

	self assert: (loop bodyInstructions count: [ :i | i = loopInstruction ]) equals: unrollingFactor
]
