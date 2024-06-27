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
	cfg := self generateDruidIRFor:
		       #primitiveLoopWithLoopInvariantInstructionInsideIf.

	loop := cfg allLoops first.

	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 5. 6. 7. 8 }.
	self
		assertCollection: (loopSubgraph collect: #id)
		hasSameElements: loopBlocksIds
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithMultipleBodyBlocks [

	| cfg loop loopSubgraph loopBlocksIds loopBodyBlock |
	cfg := self generateDruidIRFor: #primitiveLoopWithMultipleInvariants.

	loopBodyBlock := cfg blockById: 6.
	loopBodyBlock second breakBasicBlock.
	loop := cfg allLoops anyOne.

	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 5. 6. 12. 13 }.
	self
		assertCollection: (loopSubgraph collect: #id)
		hasSameElements: loopBlocksIds
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithNestedLoops [

	| cfg outerLoop loopSubgraph loopBlocksIds |
	cfg := self generateDruidIRFor: #primitiveNestedLoopWithInvariant.

	outerLoop := cfg allLoops second.

	loopSubgraph := outerLoop loopGraph.

	loopBlocksIds := { 5. 7. 8. 9 }.
	self
		assertCollection: (loopSubgraph collect: #id)
		hasSameElements: loopBlocksIds
]

{ #category : #tests }
DRLoopTest >> testCanRetrieveLoopGraphWithSingleBodyBlock [

	| cfg loop loopSubgraph loopBlocksIds |
	cfg := self generateDruidIRFor: #primitiveLoopWithInvariant.

	loop := cfg allLoops anyOne.

	loopSubgraph := loop loopGraph.

	loopBlocksIds := { 5. 6 }.
	self
		assertCollection: (loopSubgraph collect: #id)
		hasSameElements: loopBlocksIds
]

{ #category : #tests }
DRLoopTest >> testFixBackjumpsIgnoreInnerRecursivePhis [

	| cfg innerPhi loopHeaderPhi loopBody addition1 addition2 |
	cfg := self setUpCFGWithConditionalWithPhi.

	loopHeaderPhi := cfg b1 phiWithVariables: {  }.
	addition1 := cfg b2 add: loopHeaderPhi to: 7.

	innerPhi := cfg b4 phiFunctions unique.
	innerPhi replaceOperandAtIndex: 1 by: addition1.

	loopBody := cfg newBasicBlock. "5"
	addition2 := loopBody add: innerPhi to: 8.
	cfg b4 jumpIfTrueTo: loopBody ifFalseTo: cfg newBasicBlock.
	loopBody backJumpTo: cfg b1.
	loopHeaderPhi mergeOperands: { DRNullValue new. addition2 }.
	cfg validate.
	
	self assert: loopHeaderPhi hasRecursiveUse.
	self assert: innerPhi hasRecursiveUse.
	self assert: cfg allBackJumps unique equals: loopBody endInstruction.
	
	cfg fixBackJumps.

	self assert: cfg allBackJumps unique equals: loopBody endInstruction
]
