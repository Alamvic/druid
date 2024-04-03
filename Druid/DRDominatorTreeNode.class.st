Class {
	#name : #DRDominatorTreeNode,
	#superclass : #Object,
	#instVars : [
		'block',
		'children',
		'parent'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #'instance creation' }
DRDominatorTreeNode class >> new [
	| node |
	node := super new.
	node children: OrderedCollection new.
	^ node.
	
]

{ #category : #accessing }
DRDominatorTreeNode >> allChildren [
	"Cumulate all the transitive children of this node"

	^ self children , (self children flatCollect: [ :e | e allChildren ])
]

{ #category : #accessing }
DRDominatorTreeNode >> block [
	^ block
]

{ #category : #accessing }
DRDominatorTreeNode >> block: b [

	block := b
]

{ #category : #accessing }
DRDominatorTreeNode >> children [
	^ children
]

{ #category : #accessing }
DRDominatorTreeNode >> children: c [

	children := c
]

{ #category : #accessing }
DRDominatorTreeNode >> findImmediateDominator: tree [

	| nearestCommonAncestor currentPredecessor |
	nearestCommonAncestor := tree blockDictionary at:
		                         (block predecessors at: 1).
	block predecessors do: [ :p |
		currentPredecessor := tree blockDictionary at: p.
		nearestCommonAncestor := tree
			                         nearestCommonAncestorOf:
			                         nearestCommonAncestor
			                         and: currentPredecessor ].
	^ nearestCommonAncestor
]

{ #category : #testing }
DRDominatorTreeNode >> isDominatedByBlock: dominatorBlock [

	block = dominatorBlock ifTrue: [ ^ true ].
	parent ifNil: [ ^ false ].
	^ parent isDominatedByBlock: dominatorBlock
]

{ #category : #accessing }
DRDominatorTreeNode >> parent [
	^ parent
]

{ #category : #accessing }
DRDominatorTreeNode >> parent: node [
	parent := node
]

{ #category : #accessing }
DRDominatorTreeNode >> replaceParent: node [
	self parent children remove: self.
	self parent: node.
	node children addLast: self.
]
