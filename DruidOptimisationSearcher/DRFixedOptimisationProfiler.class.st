Class {
	#name : #DRFixedOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#instVars : [
		'manualOptimisations'
	],
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRFixedOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ manualOptimisations
		  at: optimisations size + 1
		  ifAbsentPut: [ DRNoOptimisation new ]
]

{ #category : #initialization }
DRFixedOptimisationProfiler >> initialize [
 

	manualOptimisations := self optimisationsToApply asOrderedCollection 
]

{ #category : #initialization }
DRFixedOptimisationProfiler >> optimisationsToApply [

	self subclassResponsibility
]
