Class {
	#name : #DRBaseOffsetMemoryAddress,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'base',
		'offset'
	],
	#category : #'Druid-IR'
}

{ #category : #'meta-data' }
DRBaseOffsetMemoryAddress >> asDependencyList [

	"Return myself as a list of dependencies, independently of MY dependencies.
	I reprensent a dependency to my base and offset"	
	^ { base. offset }
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> base [

	^ base
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> base: anObject [

	base := anObject
]

{ #category : #printing }
DRBaseOffsetMemoryAddress >> name [
	
	^ '[', base result name, ', offset=', offset name, ']'
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> offset [

	^ offset
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> offset: anObject [

	offset := anObject
]

{ #category : #cogit }
DRBaseOffsetMemoryAddress >> rtlArgumentExpressions [
	
	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: offset value.
	baseNode := RBVariableNode named: base result name.

	^ { offsetNode . baseNode }
]
