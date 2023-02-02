Class {
	#name : #DRExperimentalOptimisationProfiler,
	#superclass : #DRFixedOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #initialization }
DRExperimentalOptimisationProfiler >> optimisationsToApply [

	^ { 
		  DRCleanControlFlow.
		  DRCleanControlFlow.
		  DRCopyPropagation.
		  DRDeadCodeElimination.
		  DRBranchCollapse.
		  DRDeadCodeElimination.
		  DRSCCP.
		  DRDeadCodeElimination.
		  DRFailureCodeBasedTailDuplication.
		  DRCleanControlFlow.
		  DRCleanControlFlow.
		  DRCopyPropagation.
		  DRDeadCodeElimination.
		  DRRedundantCopyElimination.
		  DRSCCP.
		  DRDeadCodeElimination.
		  DRCleanControlFlow.
		  DRFailureCodeBasedTailDuplication.
		  DRCleanControlFlow.
		  DRCleanControlFlow.
		  DRSCCP.
		  DRDeadCodeElimination } collect: [ :opt | opt new ]
]
