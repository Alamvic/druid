Class {
	#name : 'DRControlFlowOptimisation',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRControlFlowOptimisation >> applyTo: cfg [

	cfg rebuildStackDependencies.
	self doApply: cfg.
	DRDeadBlockElimination applyTo: cfg.
	cfg fixBackJumps
]

{ #category : 'accessing' }
DRControlFlowOptimisation >> doApply: cfg [

	self subclassResponsibility
]
