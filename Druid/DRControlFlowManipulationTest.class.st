Class {
	#name : #DRControlFlowManipulationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRControlFlowManipulationTest >> testRemoveInstructionWithDuplicatedOperandRemovesBackpointer [

	| cfg r0 r1 |
	cfg := self setUpCFGWithConditional.
	
	r0 := cfg b1 copy: 1.
	r1 := cfg b4 phiWith: r0 with: r0.
	
	r1 removeFromCFG.
	
	self assert: r0 users isEmpty
]

{ #category : #tests }
DRControlFlowManipulationTest >> testRemovePredecessorRemovesPhiArgument [

	| cfg comingFrom1 comingFrom2 phiFunction |
	cfg := self setUpCFGWithConditional.
	comingFrom1 := cfg b2 copy: 1.
	comingFrom2 := cfg b3 copy: 2.
	"R0 := Phi(comingFrom1, comingFrom2)"
	phiFunction := cfg b4 phiWith: comingFrom1 with: comingFrom2.

	cfg b4 removePredecessor: cfg b2.

	self
		assertCollection: phiFunction operands
		hasSameElements: { comingFrom2 }.
	self assert: (comingFrom2 users includes: phiFunction).
	self deny: (comingFrom1 users includes: phiFunction)
]

{ #category : #tests }
DRControlFlowManipulationTest >> testRemovePredecessorWithRedundantPhiKeepsPhiOperandUser [

	| cfg comingFromTop phiFunction |
	cfg := self setUpCFGWithConditional.
	comingFromTop := cfg b1 copy: 1.
	phiFunction := cfg b4 phiWith: comingFromTop with: comingFromTop.

	cfg b4 removePredecessor: cfg b2.

	self assert: phiFunction operands size equals: 1.
	self
		assertCollection: phiFunction operands
		hasSameElements: { comingFromTop }.
	self assert: (comingFromTop users includes: phiFunction)
]
