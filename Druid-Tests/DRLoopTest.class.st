Class {
	#name : #DRLoopTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #running }
DRLoopTest >> optimize: cfg [

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new)
]

{ #category : #running }
DRLoopTest >> setUp [

	super setUp.

	self configureInterpreter.
	compilerCompiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new.
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithIfs [

	| cfg loop loopSubgraph loopBlocksIds |
	cfg := self generateDruidIRFor: #primitiveLoopWithLoopInvariantInstructionInsideIf.

	loop := cfg allLoops first.
	
	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 6. 7. 8. 9 }.
	self assertCollection: (loopSubgraph blocks collect: #id) hasSameElements: loopBlocksIds.
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithMultipleBodyBlocks [

	| cfg loop loopSubgraph loopBlocksIds loopBodyBlock |
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	loopBodyBlock := cfg blockById: 7.
	loopBodyBlock second breakBasicBlock.
	loop := cfg allLoops anyOne.
	
	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 6. 9. 10. 7 }.
	self assertCollection: (loopSubgraph blocks collect: #id) hasSameElements: loopBlocksIds.
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithNestedLoops [

	| cfg outerLoop loopSubgraph loopBlocksIds |
	cfg := self generateDruidIRFor: #primitiveNestedLoopWithInvariant.

	outerLoop := cfg allLoops first.
	
	loopSubgraph := outerLoop loopGraph.

	loopBlocksIds := { 6. 8. 9. 10 }.
	self assertCollection: (loopSubgraph blocks collect: #id) hasSameElements: loopBlocksIds.
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithSingleBodyBlock [

	| cfg loop loopSubgraph loopBlocksIds |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	loop := cfg allLoops anyOne.
	
	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 6. 7 }.
	self assertCollection: (loopSubgraph blocks collect: #id) hasSameElements: loopBlocksIds.
]
