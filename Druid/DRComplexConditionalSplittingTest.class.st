Class {
	#name : #DRComplexConditionalSplittingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRComplexConditionalSplittingTest >> previousOptimizations [

	^ { DRCopyPropagation new }
]

{ #category : #running }
DRComplexConditionalSplittingTest >> setUp [

	super setUp.

	optimisation := DRComplexConditionalSplitting new
]

{ #category : #tests }
DRComplexConditionalSplittingTest >> testSplitAnd [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAndConditionOnSameVariable.
	
	cfg applyOptimization: optimisation.

	self assert: cfg allConditionalJumps size equals: 2 + 1 "Last jump: ok or fail"

]

{ #category : #tests }
DRComplexConditionalSplittingTest >> testSplitMultipleAnd [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveBranchingWithTripleAnd.
	
	cfg applyOptimization: optimisation.

	self assert: cfg allConditionalJumps size equals: 3 + 1 "Last jump: ok or fail"

]

{ #category : #tests }
DRComplexConditionalSplittingTest >> testSplitOr [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveBranchingWithOrConditionOnSameVariable.
	
	cfg applyOptimization: optimisation.

	self assert: cfg allConditionalJumps size equals: 2 + 1 "Last jump: ok or fail"

]

{ #category : #tests }
DRComplexConditionalSplittingTest >> testSplitSimpleCondition [

	| cfg |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAssigments.
	
	cfg applyOptimization: optimisation.

	self assert: cfg allConditionalJumps size equals: 1 + 1 "Last jump: ok or fail"

]
