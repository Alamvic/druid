Class {
	#name : #DRAbsoluteMemoryAddress,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'address'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRAbsoluteMemoryAddress class >> register: aRegister [
	
	^ self new
		register: aRegister;
		yourself
]

{ #category : #accessing }
DRAbsoluteMemoryAddress >> address [

	^ address
]

{ #category : #accessing }
DRAbsoluteMemoryAddress >> address: anObject [

	address := anObject
]

{ #category : #cogit }
DRAbsoluteMemoryAddress >> rtlArgumentExpressions [

	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: 0.
	baseNode := RBVariableNode named: address result name.

	^ { offsetNode . baseNode }
]
