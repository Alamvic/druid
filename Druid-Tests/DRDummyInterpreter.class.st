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
	"Answer a new <AbstractInterpreter>"

	^ DREmptyInterpreter basicNew
]

{ #category : #initialization }
DRDummyInterpreter >> initializeWithOptions: aCollection objectMemoryClass: aClass [ 


]

{ #category : #accessing }
DRDummyInterpreter >> primitiveTable [
	"Answer a <Collection> of <Symbol> specifying primitive table method selectors. If none is found, use the primitives specified in the receiver's"

	^ primitiveTable
		ifNil: [ primitiveTable := self basicNew basicPrimitiveTable ]
]

{ #category : #private }
DRDummyInterpreter >> primitives: aCollection [

	primitiveTable := aCollection
]
