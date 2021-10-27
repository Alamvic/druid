Class {
	#name : #DRControlFlowGraphForTesting,
	#superclass : #DRControlFlowGraph,
	#instVars : [
		'b0',
		'b1',
		'b2',
		'b3',
		'b4',
		'ConditionnalJump'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRControlFlowGraphForTesting >> b0 [

	^ b0
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b0: aBlock [

	initialBasicBlock := aBlock.
	b0 := aBlock
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b1 [

	^ b1
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b1: aBlock [

	b1 := aBlock
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b2 [

	^ b2
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b2: aBlock [

	b2 := aBlock
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b3 [

	^ b3
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b3: aBlock [

	b3 := aBlock
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b4 [ 

	^ b4
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> b4: aBlock [ 

	b4 := aBlock
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> conditionnalJump [

	^ ConditionnalJump
]

{ #category : #accessing }
DRControlFlowGraphForTesting >> conditionnalJump: CJ [

	ConditionnalJump := CJ
]
