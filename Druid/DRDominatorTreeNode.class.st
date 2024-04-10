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
DRDominatorTreeNode >> addChild: aChild [

	children addLast: aChild
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

{ #category : #printing }
DRDominatorTreeNode >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.
	aStream
		nextPutAll: ' block: ';
		print: block
]

{ #category : #accessing }
DRDominatorTreeNode >> replaceParent: node [
	self parent children remove: self.
	self parent: node.
	node children addLast: self.
]
