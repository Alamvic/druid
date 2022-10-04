Class {
	#name : #DRExperimentalOptimisationProfiler,
	#superclass : #DRFixedOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #initialization }
DRExperimentalOptimisationProfiler >> optimisationsToApply [

	^ { 
		  DRCleanControlFlow.
		  DRCopyPropagation.
		  DRCleanControlFlow.
		  DRDeadCodeElimination.
		  DRSCCP.
		  DRDeadCodeElimination.
		  DRBranchCollapse.
		  DRDeadCodeElimination.
		  DRDeadEdgeSplitting.
		  DRCleanControlFlow.
		  DRDeadBranchElimination.
		  DRCopyPropagation.
		  DRDeadCodeElimination.
		  DRRedundantCopyElimination.
		  DRCleanControlFlow.
		  DRFailureCodeBasedTailDuplication.
		  DRCleanControlFlow.
		  DRSCCP.
		  DRCleanControlFlow.
		  DRDeadCodeElimination } collect: [ :opt | opt new ]
]
