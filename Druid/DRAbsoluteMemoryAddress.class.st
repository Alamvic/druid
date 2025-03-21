Class {
	#name : 'DRAbsoluteMemoryAddress',
	#superclass : 'DRAbstractMemoryAddress',
	#instVars : [
		'expression'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRAbsoluteMemoryAddress class >> register: aRegister [
	
	^ self new
		register: aRegister;
		yourself
]

{ #category : 'visiting' }
DRAbsoluteMemoryAddress >> acceptVisitor: aVisitor [ 
	
	^ aVisitor visitAbsoluteMemoryAddress: self
]

{ #category : 'meta-data' }
DRAbsoluteMemoryAddress >> asDependencyList [

	"Return myself as a list of dependencies, independently of MY dependencies.
	I reprensent a dependency to my expression"	
	^ { expression }
]

{ #category : 'accessing' }
DRAbsoluteMemoryAddress >> expression [
	
	^ expression
]

{ #category : 'accessing' }
DRAbsoluteMemoryAddress >> expression: anObject [

	expression := anObject
]

{ #category : 'testing' }
DRAbsoluteMemoryAddress >> isAbsolute [
	
	^ true
]

{ #category : 'testing' }
DRAbsoluteMemoryAddress >> isStaged [
	
	^ expression isStaged
]

{ #category : 'testing' }
DRAbsoluteMemoryAddress >> name [

	^ '[' , expression result prettyPrint , ']'
]

{ #category : 'accessing' }
DRAbsoluteMemoryAddress >> operands [

	^ { expression }
]

{ #category : 'users-definitions' }
DRAbsoluteMemoryAddress >> replaceDependency: anOperand by: anotherOperand [

	expression = anOperand
		ifTrue: [ ^ self expression: anotherOperand ].
	
	self error: 'Invalid dependency'
]

{ #category : 'cogit' }
DRAbsoluteMemoryAddress >> rtlArgumentExpressions: generator [

	^ expression rtlArgumentsForAbsoluteAddressForGenerator: generator
]

{ #category : 'meta-data' }
DRAbsoluteMemoryAddress >> rtlOperandQualifierForInstruction: instruction [

	^ expression rtlOperandQualifierForAbsoluteAddress
]

{ #category : 'SCCP' }
DRAbsoluteMemoryAddress >> sccpLatticeValueFor: sccp [
	
	^ sccp latticeValue: expression
]

{ #category : 'accessing' }
DRAbsoluteMemoryAddress >> type [

	^ expression type
]
