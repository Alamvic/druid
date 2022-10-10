Class {
	#name : #DRBestFoundOptimisationProfiler,
	#superclass : #DRSearchByHeuristicOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRBestFoundOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	| bestOptimisation bestInstructionCount |
	bestOptimisation := nil.
	bestInstructionCount := instructionCount.
	self optimisationList do: [ :potentialMin | 
		| ir |
		ir := self newIRFor: aSelector withOptimisations: optimisations , { potentialMin } , postSequence.
		ir instructions size < bestInstructionCount ifTrue: [ 
			bestOptimisation := potentialMin.
			bestInstructionCount := ir instructions size ] ].

	^ bestOptimisation
]
