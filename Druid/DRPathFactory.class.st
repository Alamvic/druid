Class {
	#name : #DRPathFactory,
	#superclass : #Object,
	#instVars : [
		'cfg'
	],
	#category : #'Druid-IR-Paths'
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
DRPathFactory >> createAllEdgesFrom: source to: destination branchIndex: branchIndex [

	| edge |
	(cfg incomingEdgesFor: source) do: [ :e | "We should generate one outgoing for each incoming edge"
		edge := cfg addEdgeFrom: source to: destination branchIndex: branchIndex.
		edge parent: e.
		source instructions do: [ :i | i acceptVisitor: edge ] ]
]

{ #category : #building }
DRPathFactory >> createInitialEdge [

	| initialBlock initialEdge |
	initialBlock := cfg initialBasicBlock.
	initialEdge := cfg addEdgeFrom: initialBlock to: initialBlock successor branchIndex: 1.
	cfg initialContrainst ifNotNil: [ :constraint | initialEdge addConstraint: constraint ]
	
]

{ #category : #building }
DRPathFactory >> generatePaths [

	self createInitialEdge.
	cfg reversePostOrderBlocks allButFirst do: [ :source |
		"Create edge(s) from source block to sucessor(s)"
		source successors withIndexDo: [ :destination :index | 
			self createAllEdgesFrom: source to: destination branchIndex: index ] ]
]
