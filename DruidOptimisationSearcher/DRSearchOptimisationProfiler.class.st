Class {
	#name : #DRSearchOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRSearchOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations instructionCount: instructionCount [

	optimisations ifNotEmpty: [ 
		optimisations last isNoOptimisation ifTrue: [ 
			"If last optimisation was No-Optimsation, so we arrive to optimal solution" 
			^ DRNoOptimisation new ] ].

	^ super
		  bestOptimisationForSelector: aSelector
		  optimisationSequence: optimisations
		  instructionCount: instructionCount
]
