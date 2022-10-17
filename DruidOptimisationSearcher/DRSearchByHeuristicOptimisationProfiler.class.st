Class {
	#name : #DRSearchByHeuristicOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#instVars : [
		'level'
	],
	#category : #DruidOptimisationSearcher
}

{ #category : #'as yet unclassified' }
DRSearchByHeuristicOptimisationProfiler class >> withLevel: level [

	^ self new
		  level: level;
		  yourself
]

{ #category : #searching }
DRSearchByHeuristicOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations instructionCount: instructionCount [

	"If last optimisation was No-Optimsation, so we already arrived to optimal solution" 
	optimisations ifNotEmpty: [ 
		optimisations last isNoOptimisation ifTrue: [ 
			^ DRNoOptimisation new ] ].

	"Remove instructions"
	level >= 1 ifTrue: [ 
		(self
			 bestOptimisationForSelector: aSelector
			 optimisationSequence: optimisations
			 postOptimisationSequence: #(  )
			 instructionCount: instructionCount) ifNotNil: [ :optimisation | ^ optimisation ] ].

	"Left unused instructions"
	level >= 2 ifTrue: [ 
		(self
			 bestOptimisationForSelector: aSelector
			 optimisationSequence: optimisations
			 postOptimisationSequence: { DRDeadCodeElimination new }
			 instructionCount: instructionCount) ifNotNil: [ :optimisation | ^ optimisation ] ].

	"Code duplication"
	level >= 3 ifTrue: [ 
		(self
			 bestOptimisationForSelector: aSelector
			 optimisationSequence: optimisations
			 postOptimisationSequence: { 
					 (DRSCCP then: DRCopyPropagation).
					 (DRDeadCodeElimination then: DRCleanControlFlow) }
			 instructionCount: instructionCount) ifNotNil: [ :optimisation | ^ optimisation ] ].

	"Finish"
	^ DRNoOptimisation new
]

{ #category : #initialization }
DRSearchByHeuristicOptimisationProfiler >> initialize [

	super initialize.
	level := 5
]

{ #category : #accessing }
DRSearchByHeuristicOptimisationProfiler >> level: anInteger [

	level := anInteger
]

{ #category : #printing }
DRSearchByHeuristicOptimisationProfiler >> prettyPrint [

	^ super prettyPrint , '(' , level asString , ')'
]
