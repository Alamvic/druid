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
		  DRDeadCodeElimination.
		  DRSCCP.
		  DRDeadCodeElimination.
		  DRBranchCollapse.
		  DRDeadCodeElimination.
		  DRFailureCodeBasedTailDuplication.
		  DRCleanControlFlow.
		  DRDeadBranchElimination.
		  DRCleanControlFlow.
		  DRCopyPropagation.
		  DRDeadCodeElimination.
		  DRRedundantCopyElimination.
		  DRCleanControlFlow } collect: [ :opt | opt new ]
]
