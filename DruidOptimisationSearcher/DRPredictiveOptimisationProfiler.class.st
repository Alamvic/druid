Class {
	#name : #DRPredictiveOptimisationProfiler,
	#superclass : #DRFixedOptimisationProfiler,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRPredictiveOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	| newOptimisations futureIR currentIR |
	(super
		 bestOptimisationForSelector: aSelector
		 optimisationSequence: optimisations
		 postOptimisationSequence: postSequence
		 instructionCount: instructionCount) ifNotNil: [ :opt | ^ opt ].

	"Refil or stop"
	currentIR := self newIRFor: aSelector withOptimisations: optimisations.
	newOptimisations := self newOptimisationListFor: currentIR.

	futureIR := self
		            newIRFor: aSelector
		            withOptimisations: optimisations , newOptimisations , postSequence.

	futureIR instructions size < instructionCount ifFalse: [ ^ DRNoOptimisation new ].

	fixedOptimisations addAll: newOptimisations.
	^ super
		  bestOptimisationForSelector: aSelector
		  optimisationSequence: optimisations
		  postOptimisationSequence: postSequence
		  instructionCount: instructionCount
]

{ #category : #'optimisation - search' }
DRPredictiveOptimisationProfiler >> newOptimisationListFor: anIR [ 

	^ self optimisationList select: [ :opt | opt canBeAppliedIn: anIR  ]
]

{ #category : #initialization }
DRPredictiveOptimisationProfiler >> optimisationsToApply [

	^ {  }
]
