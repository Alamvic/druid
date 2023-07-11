Class {
	#name : #DRRegisterMemoryAddress,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'register'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRRegisterMemoryAddress class >> register: aRegister [
	
	^ self new
		register: aRegister;
		yourself
]

{ #category : #visiting }
DRRegisterMemoryAddress >> acceptVisitor: aVisitor [ 

	^ aVisitor visitRegisterAddress: self
]

{ #category : #accessing }
DRRegisterMemoryAddress >> name [

	^ register name
]

{ #category : #accessing }
DRRegisterMemoryAddress >> operands [
	
	^ #()
]

{ #category : #accessing }
DRRegisterMemoryAddress >> register [

	^ register
]

{ #category : #accessing }
DRRegisterMemoryAddress >> register: anObject [

	register := anObject
]

{ #category : #cogit }
DRRegisterMemoryAddress >> rtlArgumentExpressions: generator [

	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: 0.
	baseNode := RBVariableNode named: register name.

	^ { offsetNode . baseNode }
]

{ #category : #SCCP }
DRRegisterMemoryAddress >> sccpLatticeValueFor: aDRSCCP [

	^ aDRSCCP bottom
]
