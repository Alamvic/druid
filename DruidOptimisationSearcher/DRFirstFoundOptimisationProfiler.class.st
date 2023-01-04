Class {
	#name : #DRFirstFoundOptimisationProfiler,
	#superclass : #DRSearchByHeuristicOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRFirstFoundOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ self optimisationList
		  detect: [ :aOptimisation | 
			  | ir |
			  "Get a fresh IR and prepare it with a list of previously applied optimisations"
			  ir := self newIRFor: aSelector.
			  optimisations do: [ :e | ir applyOptimisation: e ].
			  ir applyOptimisation: aOptimisation.
			  postSequence do: [ :e | ir applyOptimisation: e ].
			  ir instructions size < instructionCount ]
		  ifNone: [ nil ]
]
