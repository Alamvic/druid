Class {
	#name : #DRSpilledMemoryAddress,
	#superclass : #DRValue,
	#instVars : [
		'stackOffset'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'as yet unclassified' }
DRSpilledMemoryAddress class >> stackOffset: offset [

	^ self new
		stackOffset: offset;
		yourself
]

{ #category : #memory }
DRSpilledMemoryAddress >> asMemoryAddress [
	
	^ self
]

{ #category : #testing }
DRSpilledMemoryAddress >> isMemoryAddress [
	
	^ true
]

{ #category : #printing }
DRSpilledMemoryAddress >> name [
	
	^ 'StackEntry-', stackOffset asString
]

{ #category : #memory }
DRSpilledMemoryAddress >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: '('.
	aStream print: self name.
	aStream nextPutAll: ')'.
]

{ #category : #cogit }
DRSpilledMemoryAddress >> rtlArgumentExpressions [

	^ { 
		  (RBLiteralValueNode value: self stackOffset * 8). "bytes"
		  (RBVariableNode named: 'SPReg') }
]

{ #category : #'cogit-generation' }
DRSpilledMemoryAddress >> rtlOperandQualifier [
	
	^ 'Mw:r:'
]

{ #category : #accessing }
DRSpilledMemoryAddress >> stackOffset [
	
	^ stackOffset
]

{ #category : #accessing }
DRSpilledMemoryAddress >> stackOffset: offset [ 
	stackOffset := offset
]
