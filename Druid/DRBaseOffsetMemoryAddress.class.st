Class {
	#name : #DRBaseOffsetMemoryAddress,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'base',
		'offset'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> base [

	^ base
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> base: anObject [

	base := anObject
]

{ #category : #'meta-data' }
DRBaseOffsetMemoryAddress >> dependencies [

	^ { base. offset }
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
