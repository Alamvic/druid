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

{ #category : #testing }
DRSpilledMemoryAddress >> isMemoryAddress [
	
	^ true
]

{ #category : #printing }
DRSpilledMemoryAddress >> name [
	
	^ 'M0'
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
