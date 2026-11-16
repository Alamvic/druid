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
DRTemporaryVariableInstruction >> address [

	^ self operand1
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> address: aDRValue [

	self replaceOperandAtIndex: 1 by: aDRValue
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> isInTempVector [

	^ self scope isInTempVector: self variableName 
]

{ #category : 'testing' }
DRTemporaryVariableInstruction >> isScopedInstruction [

	^ true
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> scope [

	^ scope
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> scope: anObject [

	scope := anObject
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> variableName [

	^ self address value
]
