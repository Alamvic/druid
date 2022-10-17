Class {
	#name : #DRFixedOptimisationProfiler,
	#superclass : #DruidOptimisationProfiler,
	#instVars : [
		'fixedOptimisations'
	],
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DRFixedOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	^ fixedOptimisations at: optimisations size + 1 ifAbsent: [ ^ nil ]
]

{ #category : #initialization }
DRFixedOptimisationProfiler >> initialize [
 

	fixedOptimisations := self optimisationsToApply asOrderedCollection 
]

{ #category : #initialization }
DRFixedOptimisationProfiler >> optimisationsToApply [

	self subclassResponsibility
]
