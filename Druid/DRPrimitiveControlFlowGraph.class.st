"
A primitive control flow graph is a control flow graph that has primitive information.
For example, primitives have two exit blocks and not one.
A backend may benefit from this information and order blocks differently.
"
Class {
	#name : 'DRPrimitiveControlFlowGraph',
	#superclass : 'DRControlFlowGraph',
	#instVars : [
		'successExitBlock',
		'primitiveExitBlock'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> failureExitBlock [

	| blocks |
	blocks := self failureExitBlocks.
	self assert: blocks size = 1.
	^ blocks anyOne
]

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> failureExitBlocks [

	^ self blocks select: [ :e | e endInstruction isPrimitiveFail ]
]

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> primitiveExitBlock [

	^ primitiveExitBlock
]

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> primitiveExitBlock: aBlock [

	primitiveExitBlock := aBlock
]

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> successExitBlock [

	^ successExitBlock
]

{ #category : 'accessing' }
DRPrimitiveControlFlowGraph >> successExitBlock: anObject [

	successExitBlock := anObject
]
