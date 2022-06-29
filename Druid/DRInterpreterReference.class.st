Class {
	#name : #DRInterpreterReference,
	#superclass : #DRAbstractMemoryAddress,
	#instVars : [
		'reference'
	],
	#category : #'Druid-IR'
}

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

{ #category : #'as yet unclassified' }
DRInterpreterReference >> name [

	^ reference
]

{ #category : #accssing }
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
