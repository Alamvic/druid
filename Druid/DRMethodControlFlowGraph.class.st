Class {
	#name : 'DRMethodControlFlowGraph',
	#superclass : 'DRControlFlowGraph',
	#instVars : [
		'scope'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope [

	^ scope
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope: anObject [

	scope ifNil: [ scope := anObject ]
]
