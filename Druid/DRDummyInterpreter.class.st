Class {
	#name : #DRDummyInterpreter,
	#superclass : #Object,
	#instVars : [
		'primitiveTable'
	],
	#category : #'Druid-Tests'
}

{ #category : #'instance creation' }
DRDummyInterpreter >> basicNew [

	^ DREmptyInterpreter basicNew
]

{ #category : #initialization }
DRDummyInterpreter >> initializeWithOptions: aCollection objectMemoryClass: aClass [ 


]

{ #category : #accessing }
DRDummyInterpreter >> primitiveTable [

	^ primitiveTable

]

{ #category : #private }
DRDummyInterpreter >> primitives: aCollection [

	primitiveTable := aCollection
]
