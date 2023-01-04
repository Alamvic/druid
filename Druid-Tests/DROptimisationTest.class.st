Class {
	#name : #DROptimisationTest,
	#superclass : #DRIRTest,
	#instVars : [
		'optimisation'
	],
	#category : #'Druid-Tests'
}

{ #category : #testing }
DROptimisationTest class >> isAbstract [

	^ self class == DROptimisationTest
]

{ #category : #'generation-IR' }
DROptimisationTest >> generateDruidIRFor: methodSelector [

	| cfg |
	cfg := super generateDruidIRFor: methodSelector.
	2 timesRepeat: [ self optimize: cfg ].
	^ cfg
]

{ #category : #optimising }
DROptimisationTest >> optimisation [

	^ optimisation
]

{ #category : #optimising }
DROptimisationTest >> optimisation: anObject [

	optimisation := anObject
]

{ #category : #optimising }
DROptimisationTest >> optimize: anIR [

	self previousOptimizations do: [ :optimization | 
		anIR applyOptimisation: optimization ]
]

{ #category : #optimising }
DROptimisationTest >> previousOptimizations [

	^ {  }
]
