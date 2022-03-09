Class {
	#name : #DRPathFactory,
	#superclass : #Object,
	#instVars : [
		'cfg'
	],
	#category : #'Druid-IR'
}

{ #category : #'as yet unclassified' }
DRPathFactory class >> withCFG: aCfg [

	^ self new
		  cfg: aCfg;
		  yourself
]

{ #category : #accessing }
DRPathFactory >> cfg [

	^ cfg
]

{ #category : #accessing }
DRPathFactory >> cfg: anObject [

	cfg := anObject
]

{ #category : #building }
DRPathFactory >> createAllEdgesFrom: source to: destination [

	| edge |
	(cfg incomingEdgesFor: source) do: [ :e | "We should generate one outgoing for each incoming edge"
		edge := cfg addEdgeFrom: source to: destination.
		edge constraints: e constraints copy.
		source instructions do: [ :i | i acceptVisitor: edge ] ]
]

{ #category : #building }
DRPathFactory >> createInitialEdge [

	| initialBlock |
	initialBlock := cfg initialBasicBlock.
	cfg addEdgeFrom: initialBlock to: initialBlock successor
]

{ #category : #building }
DRPathFactory >> generatePaths [

	self createInitialEdge.
	cfg reversePostOrderBlocks allButFirst do: [ :source | "Create edge(s) from source block to sucessor(s)"
		source successors do: [ :destination | 
			self createAllEdgesFrom: source to: destination ] ]
]
