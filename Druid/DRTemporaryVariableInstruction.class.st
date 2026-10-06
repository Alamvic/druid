Class {
	#name : 'DRTemporaryVariableInstruction',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope',
		'variableName'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> address [

	^ self variableName
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> address: aDRValue [

	self replaceOperandAtIndex: 1 by: aDRValue
]

{ #category : 'testing' }
DRTemporaryVariableInstruction >> isMandatory [

	^ true
]

{ #category : 'testing' }
DRTemporaryVariableInstruction >> isScopedInstruction [

	^ true
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> operands: aCollection [

	variableName := aCollection first.
	super operands: aCollection allButFirst
]

{ #category : 'accessing' }
DRTemporaryVariableInstruction >> prettyPrint [

	^ super prettyPrint , ' ' , variableName asString
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

	^ variableName
]
