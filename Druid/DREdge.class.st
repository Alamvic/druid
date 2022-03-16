Class {
	#name : #DREdge,
	#superclass : #Object,
	#instVars : [
		'destination',
		'source'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'as yet unclassified' }
DREdge class >> withSource: aSource andDestination: aDestination [

	^ self new
		  source: aSource;
		  destination: aDestination;
		  yourself
]

{ #category : #accessing }
DREdge >> destination [

	^ destination
]

{ #category : #accessing }
DREdge >> destination: anObject [

	destination := anObject
]

{ #category : #accessing }
DREdge >> source [

	^ source
]

{ #category : #accessing }
DREdge >> source: anObject [

	source := anObject
]
