Class {
	#name : 'DRPathSplitting',
	#superclass : 'DRPathOptimisation',
	#instVars : [
		'pathIndex'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPathSplitting class >> index: pathIndex [

	^ self new
		  pathIndex: pathIndex;
		  yourself
]

{ #category : 'accessing' }
DRPathSplitting >> doApplyWithGeneratedPaths: cfg [

	"Split all blocks in selected index path"

	| path |
	path := cfg pathAt: pathIndex.

	path reversed do: [ :edge | 
		edge destination predecessors size > 1 ifTrue: [ edge split ] ]
]

{ #category : 'accessing' }
DRPathSplitting >> pathIndex [

	^ pathIndex
]

{ #category : 'accessing' }
DRPathSplitting >> pathIndex: anObject [

	pathIndex := anObject
]
