Class {
	#name : 'DRBCTemporaryVariableInstruction',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope',
		'variableName'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> address [

	^ self variableName
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> address: aDRValue [

	self replaceOperandAtIndex: 1 by: aDRValue
]

{ #category : 'testing' }
DRBCTemporaryVariableInstruction >> isMandatory [

	^ true
]

{ #category : 'testing' }
DRBCTemporaryVariableInstruction >> isScopedInstruction [

	^ true
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> operands: aCollection [

	variableName := aCollection first.
	super operands: aCollection allButFirst
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> prettyPrint [

	^ super prettyPrint , ' ' , variableName asString
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> scope [

	^ scope
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> scope: anObject [

	scope := anObject
]

{ #category : 'accessing' }
DRBCTemporaryVariableInstruction >> variableName [

	^ variableName
]
