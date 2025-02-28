Class {
	#name : 'DRTailDuplicateStagedExpressionsTest',
	#superclass : 'DROptimisationTest',
	#category : 'Druid-Tests-Optimizations',
	#package : 'Druid-Tests',
	#tag : 'Optimizations'
}

{ #category : 'running' }
DRTailDuplicateStagedExpressionsTest >> setUp [

	super setUp.
	optimisation := DRCogitStager then: DRIsolateStagedJumps
]

{ #category : 'tests' }
DRTailDuplicateStagedExpressionsTest >> testNotTailDuplicatePhi [

	| cfg nBlocks |
	cfg := self setUpCFGWithConditionalWithPhi.

	cfg b2 firstInstruction replaceBy:
		(cfg b2 copy: (DRPhysicalGeneralPurposeRegister name: 'REG')).

	nBlocks := cfg blocks size.

	self deny: cfg b4 phiFunctions unique isJITCompileTimeExpression.

	optimisation applyTo: cfg.

	"No effect"
	self assert: cfg blocks size equals: nBlocks
]

{ #category : 'tests' }
DRTailDuplicateStagedExpressionsTest >> testTailDuplicatePhi [

	| cfg nBlocks |
	cfg := self setUpCFGWithConditionalWithPhi.

	nBlocks := cfg blocks size.

	self assert: cfg b4 phiFunctions unique isJITCompileTimeExpression.

	optimisation applyTo: cfg.

	self assert: cfg blocks size equals: nBlocks + 1
]
