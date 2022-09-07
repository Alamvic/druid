Class {
	#name : #DRAbsoluteMemoryAddress,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'expression'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRAbsoluteMemoryAddress class >> register: aRegister [
	
	^ self new
		register: aRegister;
		yourself
]

{ #category : #'meta-data' }
DRAbsoluteMemoryAddress >> asDependencyList [

	"Return myself as a list of dependencies, independently of MY dependencies.
	I reprensent a dependency to my expression"	
	^ { expression }
]

{ #category : #accessing }
DRAbsoluteMemoryAddress >> expression [
	
	^ expression
]

{ #category : #accessing }
DRAbsoluteMemoryAddress >> expression: anObject [

	expression := anObject
]

{ #category : #testing }
DRAbsoluteMemoryAddress >> isAbsolute [
	
	^ true
]

{ #category : #testing }
DRAbsoluteMemoryAddress >> name [
	
	^ '[', expression result name, ']'
]

{ #category : #'users-definitions' }
DRAbsoluteMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	expression = anOperand
		ifTrue: [ ^ expression := anotherOperand ].
	
	self error: 'Invalid dependency'
]

{ #category : #cogit }
DRAbsoluteMemoryAddress >> rtlArgumentExpressions [

	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: 0.
	baseNode := RBVariableNode named: expression result name.

	^ { offsetNode . baseNode }
]
