Class {
	#name : #DRRandomOptimisationSearcher,
	#superclass : #DruidOptimisationSearcher,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRRandomOptimisationSearcher >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ self optimisationList atRandom
]
