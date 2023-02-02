Class {
	#name : #DRRandomOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRRandomOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ self optimisationList atRandom
]
