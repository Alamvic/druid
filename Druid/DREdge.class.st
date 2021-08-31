Class {
	#name : #DREdge,
	#superclass : #Object,
	#instVars : [
		'parent',
		'child',
		'executed'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DREdge >> child [ 

	^ child
]

{ #category : #accessing }
DREdge >> child: aBlock [

	child := aBlock
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
DREdge >> parent [

	^ parent
]

{ #category : #accessing }
DREdge >> parent: aBlock [

	parent := aBlock
]
