"
A primitive control flow graph is a control flow graph that has primitive information.
For example, primitives have two exit blocks and not one.
A backend may benefit from this information and order blocks differently.
"
Class {
	#name : #DRPrimitiveControlFlowGraph,
	#superclass : #DRControlFlowGraph,
	#instVars : [
		'successExitBlock',
		'failureExitBlock',
		'primitiveExitBlock'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> failureExitBlock [

	^ failureExitBlock
]

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> failureExitBlock: anObject [

	failureExitBlock := anObject
]

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> primitiveExitBlock [

	^ primitiveExitBlock
]

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> primitiveExitBlock: aBlock [

	primitiveExitBlock := aBlock
]

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> successExitBlock [

	^ successExitBlock
]

{ #category : #accessing }
DRPrimitiveControlFlowGraph >> successExitBlock: anObject [

	successExitBlock := anObject
]
