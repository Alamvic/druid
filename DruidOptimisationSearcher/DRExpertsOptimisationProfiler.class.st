Class {
	#name : #DRExpertsOptimisationProfiler,
	#superclass : #DRFixedOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #initialization }
DRExpertsOptimisationProfiler >> optimisationsToApply [
 
	| optimisations |
	
	optimisations := OrderedCollection new.
	optimisations add: DRCleanControlFlow new.
	 optimisations add: DRCopyPropagation new.
"	optimisations add: DRLoopInvariantCodeMotion new."

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [ 
		optimisations add: DRBranchCollapse new.
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: DRSCCP new.
		optimisations add: DRDeadBlockElimination new.
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRDeadCodeElimination new.
		optimisations add: DRRedundantCopyElimination new.
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRDeadEdgeSplitting new.
		optimisations add: DRDeadBranchElimination new.
"		optimisations add: DRInline new." ].
	
	^ optimisations
]
