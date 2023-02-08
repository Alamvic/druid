Class {
	#name : #DRInterpreterReference,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'reference'
	],
	#category : #'Druid-IR'
}

{ #category : #factory }
DRInterpreterReference class >> freeStartAddress [

	^ self reference: 'objectMemory freeStartAddress'
]

{ #category : #factory }
DRInterpreterReference class >> primitiveFailureCodeAddress [

	^ self reference: 'coInterpreter primitiveFailureCodeAddress'
]

{ #category : #accessing }
DRInterpreterReference class >> reference: aReference [

	^ self new
		reference: aReference;
		yourself
]

{ #category : #memory }
DRInterpreterReference >> asMemoryAddress [
	
	^ self
]

{ #category : #accessing }
DRInterpreterReference >> name [

	^ reference
]

{ #category : #accessing }
DRInterpreterReference >> operands [
	
	^ #()
]

{ #category : #printing }
DRInterpreterReference >> printResult [

	^ reference
]

{ #category : #accessing }
DRInterpreterReference >> reference: aString [ 
	reference := aString
]

{ #category : #cogit }
DRInterpreterReference >> rtlArgumentExpressions [

	^ {RBVariableNode named: self name}
]

{ #category : #cogit }
DRInterpreterReference >> rtlOperandQualifierForInstruction: instruction [

	^ 'Aw:'
]
