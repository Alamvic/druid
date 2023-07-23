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

{ #category : #cogit }
DRAbsoluteMemoryAddress >> base [

	^ expression
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
	
	^ '[', expression result name asString, ']'
]

{ #category : #cogit }
DRAbsoluteMemoryAddress >> offset [

	^ 0
]

{ #category : #accessing }
DRAbsoluteMemoryAddress >> operands [

	^ { expression }
]

{ #category : #'users-definitions' }
DRAbsoluteMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	expression = anOperand
		ifTrue: [ ^ self expression: anotherOperand ].
	
	self error: 'Invalid dependency'
]

{ #category : #cogit }
DRAbsoluteMemoryAddress >> rtlArgumentExpressions: generator [

	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: 0.
	baseNode := RBVariableNode named: (generator allocateVariable: expression result).

	^ { offsetNode . baseNode }
]

{ #category : #SCCP }
DRAbsoluteMemoryAddress >> sccpLatticeValueFor: sccp [
	
	^ sccp latticeValue: expression
]
