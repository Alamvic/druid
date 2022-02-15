Class {
	#name : #DRDominatorTreeNode,
	#superclass : #Object,
	#instVars : [
		'block',
		'children',
		'parent'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRDominatorTreeNode class >> new [
	| node |
	node := super new.
	node children: Dictionary new.
	^ node.
	
]

{ #category : #accessing }
DRDominatorTreeNode >> block [
	^ block
]

{ #category : #accessing }
DRDominatorTreeNode >> block: b [
	block := b.
]

{ #category : #accessing }
DRDominatorTreeNode >> children [
	^ children
]

{ #category : #accessing }
DRDominatorTreeNode >> children: c [
	children := c.
]

{ #category : #accessing }
DRDominatorTreeNode >> parent [
	^ parent
]

{ #category : #accessing }
DRDominatorTreeNode >> parent: node [
	parent := node
]
