Class {
	#name : #DRPathOptimisation,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRPathOptimisation >> doApply: cfg [

	cfg applyOptimisation: DRBranchCollapse new.
	cfg generatePaths ifFalse: [ ^ self ].
	self doApplyWithGeneratedPaths: cfg.
]

{ #category : #accessing }
DRPathOptimisation >> doApplyWithGeneratedPaths: cfg [

	self subclassResponsibility
]
