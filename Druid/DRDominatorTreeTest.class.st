Class {
	#name : #DRDominatorTreeTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRDominatorTreeTest >> testCorrectDominatorTree [
	| cfg jump dominatorTree |
	jump := DRConditionalJump operands: { DRConstantValue value: true  }.
	cfg := self setUpCFGWithConditional: jump.
	
	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	self assert: dominatorTree tree children size == 1.
]
