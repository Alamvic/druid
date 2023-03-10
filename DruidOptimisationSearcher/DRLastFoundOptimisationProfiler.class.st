Class {
	#name : #DRLastFoundOptimisationProfiler,
	#superclass : #DRSearchByHeuristicOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRLastFoundOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

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
			bestOptimisation := potentialMin ] ].

	^ bestOptimisation
]
