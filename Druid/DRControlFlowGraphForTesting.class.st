Class {
	#name : 'DRControlFlowGraphForTesting',
	#superclass : 'DRPrimitiveControlFlowGraph',
	#instVars : [
		'b0',
		'b1',
		'b2',
		'b3',
		'b4',
		'phi',
		'ConditionnalJump'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'adding' }
DRControlFlowGraphForTesting >> addScopeToInstructionIfNeeded: aDRLoadTemporaryVariable [ 

	self flag: #TODO.
	"We need to re-think the CFG for testing to apply for all the hierarchy of CFG"
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b0 [

	^ b0
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b0: aBlock [

	b0 := aBlock
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b1 [

	^ b1
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b1: aBlock [

	b1 := aBlock
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b2 [

	^ b2
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b2: aBlock [

	b2 := aBlock
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b3 [

	^ b3
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b3: aBlock [

	b3 := aBlock
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b4 [ 

	^ b4
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> b4: aBlock [ 

	b4 := aBlock
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> phi [

	^ phi
]

{ #category : 'accessing' }
DRControlFlowGraphForTesting >> phi: anObject [

	phi := anObject
]
