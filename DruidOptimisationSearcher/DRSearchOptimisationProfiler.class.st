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

{ #category : #searching }
DRSearchOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	| bestOptimisation bestInstructionCount |
	bestOptimisation := nil.
	bestInstructionCount := instructionCount.
	self optimisationList do: [ :potentialMin | 
		| ir |
		"Get a fresh IR and prepare it with a list of previously applied optimisations"
		ir := self newIRFor: aSelector.
		optimisations do: [ :e | ir applyOptimisation: e ].
		ir applyOptimisation: potentialMin.
		postSequence do: [ :e | ir applyOptimisation: e ].
		ir instructions size < bestInstructionCount ifTrue: [ 
			bestOptimisation := potentialMin.
			"bestInstructionCount := ir instructions size" ] ].

	^ bestOptimisation
]
