Class {
	#name : #DRControlFlowOptimisation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRControlFlowOptimisation >> applyTo: cfg [

	| result |
	
	result := self doApply: cfg.
	(DRDeadBlockElimination then: DRPhiSimplication) applyTo: cfg.
	cfg fixBackJumps.
	
	^ result
]

{ #category : #accessing }
DRControlFlowOptimisation >> doApply: cfg [

	self subclassResponsibility
]
