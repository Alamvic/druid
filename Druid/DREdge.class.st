Class {
	#name : #DREdge,
	#superclass : #Object,
	#instVars : [
		'executed',
		'destination',
		'source'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DREdge >> destination [

	^ destination
]

{ #category : #accessing }
DREdge >> destination: anObject [

	destination := anObject
]

{ #category : #accessing }
DREdge >> executed: anObject [

	executed := anObject
]

{ #category : #accessing }
DREdge >> initialize [ 

	executed := false 
]

{ #category : #accessing }
DREdge >> isExecuted [

	^ executed
]

{ #category : #accessing }
DREdge >> mark [ 

	executed := true
]

{ #category : #accessing }
DREdge >> source [

	^ source
]

{ #category : #accessing }
DREdge >> source: anObject [

	source := anObject
]
