Class {
	#name : #DROptimisation,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DROptimisation class >> applyTo: aCfg [

	^ self new applyTo: aCfg
]

{ #category : #accessing }
DROptimisation class >> repeat: times [

	^ self new repeat: times
]

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

{ #category : #testing }
DROptimisation >> isNoOptimisation [

	^ false
]

{ #category : #printing }
DROptimisation >> prettyPrint [

	^ self className
]

{ #category : #'accessing - structure variables' }
DROptimisation >> repeat: times [

	| sequencer |
	sequencer := DROptimisationSequence new.
	times timesRepeat: [ sequencer addOptimisation: self ].
	^ sequencer
]

{ #category : #accessing }
DROptimisation >> then: anotherOptimisation [

	^ DROptimisationSequence new
		addOptimisation: self;
		addOptimisation: anotherOptimisation;
		yourself
		
]

{ #category : #visiting }
DROptimisation >> visitDeoptimize: aDRDeoptimize [ 
	
	"Do nothing by default"
]
