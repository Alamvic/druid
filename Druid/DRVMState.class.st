Class {
	#name : #DRVMState,
	#superclass : #Object,
	#instVars : [
		'operandStack',
		'primitiveFailureCode'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRVMState >> operandStack [

	^ operandStack
]

{ #category : #accessing }
DRVMState >> operandStack: anObject [

	operandStack := anObject
]

{ #category : #copying }
DRVMState >> postCopy [
	super postCopy.
	operandStack := operandStack copy
]

{ #category : #accessing }
DRVMState >> primitiveFailureCode [

	^ primitiveFailureCode
]

{ #category : #accessing }
DRVMState >> primitiveFailureCode: anObject [

	primitiveFailureCode := anObject
]
