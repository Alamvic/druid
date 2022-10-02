Class {
	#name : #DRManualOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#instVars : [
		'manualOptimisations'
	],
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRManualOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ manualOptimisations
		  at: optimisations size + 1
		  ifAbsentPut: [ DRNoOptimisation new ]
]

{ #category : #initialization }
DRManualOptimisationProfiler >> initialize [
 
	| optimisations |
	
	optimisations := OrderedCollection new.
	optimisations add: DRCleanControlFlow new.
	 optimisations add: DRCopyPropagation new.
	optimisations add: DRLoopInvariantCodeMotion new.

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
		optimisations add: DRInline new. ].
	
	manualOptimisations := optimisations
]
