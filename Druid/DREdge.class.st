Class {
	#name : 'DREdge',
	#superclass : 'Object',
	#instVars : [
		'destination',
		'source'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'as yet unclassified' }
DREdge class >> withSource: aSource andDestination: aDestination [

	^ self new
		  source: aSource;
		  destination: aDestination;
		  yourself
]

{ #category : 'accessing' }
DREdge >> destination [

	^ destination
]

{ #category : 'accessing' }
DREdge >> destination: anObject [

	destination := anObject
]

{ #category : 'accessing' }
DREdge >> isBackJump [

	^ source lastInstruction isBackJumpTo: destination 
]

{ #category : 'deleting' }
DREdge >> kill [

	| newSucessor |
	"Precondition: source has 2 successors
	Does it make sense to kill an edge from a block with only 1 successor?
	PiNodes shouldn't generate such situation anyways, because they are inserted at split points"
	
	newSucessor := (source successors copyWithout: destination) unique.
	source jumpTo: newSucessor.
	newSucessor removePiNodes
]

{ #category : 'printing' }
DREdge >> printOn: aStream [

	aStream nextPutAll: self class name asString.
	aStream nextPutAll: '('.
	aStream print: source id.
	aStream nextPutAll: ' -> '.
	aStream print: destination id.
	aStream nextPutAll: ')'
]

{ #category : 'accessing' }
DREdge >> source [

	^ source
]

{ #category : 'accessing' }
DREdge >> source: anObject [

	source := anObject
]
