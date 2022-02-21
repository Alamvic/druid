Class {
	#name : #DRDominatorTree,
	#superclass : #Object,
	#instVars : [
		'tree',
		'blockDictionary'
	],
	#category : #'Druid-IR'
}

{ #category : #'as yet unclassified' }
DRDominatorTree >> dominatorTreeOf: cfg [

	| hasChanged currentNode currentPredecessor nearestCommonAncestor |
	self initialize: cfg.
	hasChanged := true.
	[ hasChanged ] whileTrue: [ 
		hasChanged := false.
		cfg blocksDo: [ :b | 
			currentNode := blockDictionary at: b.
			nearestCommonAncestor := blockDictionary at: (b predecessors at: 1).
				b predecessors do: [ :p |
				currentPredecessor := blockDictionary at: p.
				nearestCommonAncestor := (self nearestCommonAncestorOf: nearestCommonAncestor and: currentPredecessor).
			].
			(currentNode parent = currentPredecessor or:
			currentNode parent = nearestCommonAncestor) ifFalse: [ 
				currentNode parent children removeKey: b.
				currentNode parent: nearestCommonAncestor.
				nearestCommonAncestor children add: b->currentNode.
				hasChanged := true.
			]
		]
	]
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> initialize: cfg [

	| currentNode workList node |
	
	blockDictionary := Dictionary new.
	tree := DRDominatorTreeNode new.
	workList := OrderedCollection new.
	workList add: tree.
	
	tree block: cfg initialBasicBlock.
	blockDictionary at: cfg initialBasicBlock put: tree.
	
	[ workList isNotEmpty ] whileTrue: [
		currentNode := workList removeAt: 1.
		currentNode block endInstruction targets do: [ :b |
			node := DRDominatorTreeNode new.
			blockDictionary at: b ifAbsent: [
				blockDictionary add: b->node.
				workList addFirst: (node block: b;
				parent: currentNode;
				yourself).
				currentNode children add: b->node.
			].
		].
	]
	
	
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> nearestCommonAncestorOf: n1 and: n2 [
	| n1Path n2Path minPathLength lastIndex |
	n1Path := self pathToRoot: n1.
	n2Path := self pathToRoot: n2.
	minPathLength := n1Path size min: n2Path size.
	1 to: minPathLength do: [ :i |
		(n1Path at: i) = (n2Path at: i) ifFalse: [ ^ n1Path at: i - 1 ].
		lastIndex := i.
	].
	^ n1Path at: (lastIndex - 1 max: 1).
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> pathToRoot: node [
	| currentNode path |
	currentNode := node.
	path := OrderedCollection new.
	path addFirst: currentNode.
	[ currentNode = tree ] whileFalse: [ 
			currentNode := currentNode parent.
			path addFirst: currentNode.
	].
	^ path
]

{ #category : #accessing }
DRDominatorTree >> tree [
	^ tree
]
