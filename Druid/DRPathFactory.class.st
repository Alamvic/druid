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
DRPathFactory >> createEdgeFromParent: parentEdge to: destination branchIndex: branchIndex [

	| source edge |
	source := parentEdge destination.
	edge := cfg
		        addEdgeFrom: source
		        to: destination
		        branchIndex: branchIndex.
	edge parent: parentEdge.
	source instructions do: [ :i | i acceptVisitor: edge ].
	^ edge
]

{ #category : #building }
DRPathFactory >> createInitialEdge [

	| initialBlock |
	initialBlock := cfg initialBasicBlock.
	cfg addEdgeFrom: initialBlock to: initialBlock successor branchIndex: 1
]

{ #category : #building }
DRPathFactory >> generatePaths [

	self createInitialEdge.

	{ cfg edges first } asOrderedCollection whileNotEmpty: [ :parentEdge :worklist :backJumps | 
		| sourceBlock |
		sourceBlock := parentEdge destination.
		sourceBlock successors withIndexDo: [ :successor :index | 
			| newEdge shouldStop |
			"Propagate edge to create the path"
			newEdge := self
				           createEdgeFromParent: parentEdge
				           to: successor
				           branchIndex: index.
			"Check stop conditions"
			shouldStop := (worklist includes: successor) or: [ 
				              backJumps includes: sourceBlock -> successor ].
			"Continue with successor"
			shouldStop ifFalse: [ worklist add: newEdge ].
			"Keep info about backJumps"
			sourceBlock endInstruction isBackJump ifTrue: [ 
				backJumps add: sourceBlock -> successor ] ] ]
]
