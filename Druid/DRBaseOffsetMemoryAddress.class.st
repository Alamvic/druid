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
	^ { base . offset asDRValue }
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
	
	^ '[', base result name, ', offset=', offset asDRValue name, ']'
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> offset [

	^ offset
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> offset: anObject [

	offset := anObject
]

{ #category : #accessing }
DRBaseOffsetMemoryAddress >> operands [

	^ { 
		  base.
		  offset }
]

{ #category : #'users-definitions' }
DRBaseOffsetMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	base = anOperand ifTrue: [ ^ base := anotherOperand ].
	super replaceDependency: anOperand by: anotherOperand
]

{ #category : #cogit }
DRBaseOffsetMemoryAddress >> rtlArgumentExpressions: generator [
	
	| offsetNode baseNode |
	offsetNode := RBParser parseExpression: offset expression.
	baseNode := RBVariableNode named: (generator allocateVariable: base result).

	^ { offsetNode . baseNode }
]

{ #category : #SCCP }
DRBaseOffsetMemoryAddress >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom "?"
]
