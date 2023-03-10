Class {
	#name : #DRControlFlowOptimisation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRControlFlowOptimisation >> applyTo: cfg [

	self doApply: cfg.
	(DRDeadBlockElimination then: DRPhiSimplication) applyTo: cfg.
	cfg fixBackJumps.
]

{ #category : #accessing }
DRControlFlowOptimisation >> doApply: cfg [

	self subclassResponsibility
]
