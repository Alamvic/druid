Class {
	#name : #DROptimisationSequence,
	#superclass : #DROptimisation,
	#instVars : [
		'sequence'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #adding }
DROptimisationSequence >> addOptimisation: anOptimisation [

	sequence add: anOptimisation
]

{ #category : #accessing }
DROptimisationSequence >> applyTo: anIR [

	sequence do: [ :each | each applyTo: anIR ]
]

{ #category : #initialization }
DROptimisationSequence >> initialize [

	super initialize.
	sequence := OrderedCollection new
]

{ #category : #accessing }
DROptimisationSequence >> then: anotherOptimisation [

	self addOptimisation: anotherOptimisation.
	^ self
]
