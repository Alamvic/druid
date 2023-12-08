Class {
	#name : #DRStoreTemporaryStagedLoopTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #running }
DRStoreTemporaryStagedLoopTest >> setUp [

	super setUp.
	optimisation := DRStoreTemporaryStagedLoop new
]

{ #category : #tests }
DRStoreTemporaryStagedLoopTest >> testNotStoreTemporaryForNotStagedLoop [

	| cfg loop inductionPhi nInstructions |
	cfg := self setUpCFGWithLoop.
	loop := cfg allLoops unique.
	inductionPhi := loop loopPhiFunctions unique.

	inductionPhi operand1
		replaceOperandAtIndex: 1
		by: (DRPhysicalGeneralPurposeRegister name: 'REG').

	self deny: inductionPhi isJITCompileTimeExpression.

	nInstructions := cfg instructions size.
	optimisation applyTo: cfg.

	self assert: cfg instructions size equals: nInstructions
]

{ #category : #tests }
DRStoreTemporaryStagedLoopTest >> testStoreTemporaryForLoopInductionVariables [

	| cfg loop inductionPhi |
	cfg := self setUpCFGWithLoop.
	loop := cfg allLoops unique.
	inductionPhi := loop loopPhiFunctions unique.

	self assert: inductionPhi isJITCompileTimeExpression.

	optimisation applyTo: cfg.

	self assert: (inductionPhi operands allSatisfy: #isStore).
	self assert: inductionPhi users unique isLoad.
	self assert: inductionPhi hasRecursiveUse.
	self
		assert: inductionPhi users unique result
		equals: inductionPhi result
]
