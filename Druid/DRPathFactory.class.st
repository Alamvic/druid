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

	| initialBlock initialEdge |
	initialBlock := cfg initialBasicBlock.
	initialEdge := cfg addEdgeFrom: initialBlock to: initialBlock successor branchIndex: 1.
	cfg initialContrainst ifNotNil: [ :constraint | initialEdge addConstraint: constraint constraint to: constraint reg name  ]
	
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
			"Mark backJumps"
			(sourceBlock endInstruction isBackJumpTo: successor) ifTrue: [ 
				backJumps add: sourceBlock -> successor ] ] ]
]
