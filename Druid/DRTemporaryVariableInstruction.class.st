Class {
	#name : 'DRTemporaryVariableInstruction',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> scope [

	^ scope
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> scope: anObject [

	scope := anObject
]
