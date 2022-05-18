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

{ #category : #accessing }
DRRegisterMemoryAddress >> register [

	^ register
]

{ #category : #accessing }
DRRegisterMemoryAddress >> register: anObject [

	register := anObject
]

{ #category : #cogit }
DRRegisterMemoryAddress >> rtlArgumentExpressions [

	| offsetNode baseNode |
	offsetNode := RBLiteralValueNode value: 0.
	baseNode := RBVariableNode named: register name.

	^ { offsetNode . baseNode }
]

{ #category : #'cogit-generation' }
DRRegisterMemoryAddress >> rtlOperandQualifierForInstruction: instruction [

	| bytesSuffix |
	bytesSuffix := instruction size = 1
		               ifTrue: [ 'b' ]
		               ifFalse: [ (instruction size * 8) asString "bytes" ].

	^ 'M' , bytesSuffix , ':r:'
]
