Class {
	#name : 'DRBaseOffsetMemoryAddress',
	#superclass : 'DRAbstractMemoryAddress',
	#instVars : [
		'base',
		'offset'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRBaseOffsetMemoryAddress >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitBaseOffsetMemoryAddress: self
]

{ #category : 'meta-data' }
DRBaseOffsetMemoryAddress >> asDependencyList [

	"Return myself as a list of dependencies, independently of MY dependencies.
	I reprensent a dependency to my base and offset"	
	^ { base . offset asDRValue }
]

{ #category : 'accessing' }
DRBaseOffsetMemoryAddress >> base [

	^ base
]

{ #category : 'accessing' }
DRBaseOffsetMemoryAddress >> base: anObject [

	base := anObject
]

{ #category : 'printing' }
DRBaseOffsetMemoryAddress >> name [

	^ '[' , base result prettyPrint , ', offset='
	  , offset asDRValue result prettyPrint , ']'
]

{ #category : 'accessing' }
DRBaseOffsetMemoryAddress >> offset [

	^ offset
]

{ #category : 'accessing' }
DRBaseOffsetMemoryAddress >> offset: anObject [

	offset := anObject
]

{ #category : 'accessing' }
DRBaseOffsetMemoryAddress >> operands [

	^ { 
		  base.
		  offset }
]

{ #category : 'users-definitions' }
DRBaseOffsetMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	base = anOperand ifTrue: [ ^ base := anotherOperand ].
	offset = anOperand ifTrue: [ ^ offset := anotherOperand ].
	
	super replaceDependency: anOperand by: anotherOperand
]

{ #category : 'cogit' }
DRBaseOffsetMemoryAddress >> rtlArgumentExpressions: generator [

	| offsetNode baseNode |
	self assert: offset isConstant.
	self assert: base isInstruction.

	offsetNode := RBParser parseExpression: offset expression.
	baseNode := RBVariableNode named: (generator allocateVariable: base result).

	"In cogit offset comes before base"
	^ {
		  offsetNode.
		  baseNode }
]

{ #category : 'SCCP' }
DRBaseOffsetMemoryAddress >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom "?"
]
