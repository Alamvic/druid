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

{ #category : #cogit }
DRInterpreterReference >> base [

	^ self name
]

{ #category : #accessing }
DRInterpreterReference >> name [

	^ reference
]

{ #category : #cogit }
DRInterpreterReference >> offset [

	^ 0
]

{ #category : #accessing }
DRInterpreterReference >> operands [
	
	^ #()
]

{ #category : #accessing }
DRInterpreterReference >> reference: aString [ 
	reference := aString
]

{ #category : #cogit }
DRInterpreterReference >> rtlArgumentExpressions: generator [

	^ {RBVariableNode named: self name}
]

{ #category : #cogit }
DRInterpreterReference >> rtlOperandQualifierForInstruction: instruction [

	^ 'Aw:'
]

{ #category : #SCCP }
DRInterpreterReference >> sccpLatticeValueFor: aDRSCCP [ 

	^ aDRSCCP bottom 
]
