Class {
	#name : #DROptimisation,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DROptimisation class >> then: anotherOptimisationClass [

	^ self new then: anotherOptimisationClass new
]

{ #category : #'as yet unclassified' }
DROptimisation class >> then: firstOptimisationClass then: secondOptimisationClass [ 
	
	^ (self then: firstOptimisationClass) then: secondOptimisationClass new
]

{ #category : #accessing }
DROptimisation >> applyTo: cfg [

	self subclassResponsibility
]

{ #category : #accessing }
DROptimisation >> then: anotherOptimisation [

	^ DROptimisationSequence new
		addOptimisation: self;
		addOptimisation: anotherOptimisation;
		yourself
		
]