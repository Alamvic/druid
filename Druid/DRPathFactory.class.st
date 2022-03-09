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
DRPathFactory >> generatePaths [

	| initialBlock |
	initialBlock := cfg initialBasicBlock.
	cfg addEdgeFrom: initialBlock to: initialBlock successor.
	cfg reversePostOrderBlocks allButFirst do: [ :source | "Create edge(s) from source block to sucessor(s)"
		source successors do: [ :destination | 
			| edge |
			edge := cfg addEdgeFrom: source to: destination.
			edge constraints: (cfg incomingEdgesFor: source) first constraints copy. "(Not) merge edges from branchs"
			source instructions do: [ :i | i acceptVisitor: edge ] ] ]
]
