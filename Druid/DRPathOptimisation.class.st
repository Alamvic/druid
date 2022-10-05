Class {
	#name : #DRPathOptimisation,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRPathOptimisation >> doApply: cfg [

	cfg generatePaths.
	self doApplyWithGeneratedPaths: cfg.
	cfg generatePaths
]

{ #category : #accessing }
DRPathOptimisation >> doApplyWithGeneratedPaths: cfg [

	self subclassResponsibility
]
