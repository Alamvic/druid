Class {
	#name : 'DRCGTemporaryVariableInstruction',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRCGTemporaryVariableInstruction >> address [

	^ self operand1
]

{ #category : 'accessing' }
DRCGTemporaryVariableInstruction >> address: aDRValue [

	self replaceOperandAtIndex: 1 by: aDRValue
]

{ #category : 'testing' }
DRCGTemporaryVariableInstruction >> isScopedInstruction [

	^ true
]

{ #category : 'accessing' }
DRCGTemporaryVariableInstruction >> scope [

	^ scope
]

{ #category : 'accessing' }
DRCGTemporaryVariableInstruction >> scope: anObject [

	scope := anObject
]

{ #category : 'accessing' }
DRCGTemporaryVariableInstruction >> variableName [

	^ self address value
]
