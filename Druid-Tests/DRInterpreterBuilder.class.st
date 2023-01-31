Class {
	#name : #DRInterpreterBuilder,
	#superclass : #Object,
	#instVars : [
		'primitiveTable',
		'targetInterpreter'
	],
	#category : #'Druid-Tests'
}

{ #category : #'instance creation' }
DRInterpreterBuilder >> basicNew [

	^ self interpreterClass basicNew
]

{ #category : #initialization }
DRInterpreterBuilder >> initializeWithOptions: aCollection objectMemoryClass: aClass [ 


]

{ #category : #'instance creation' }
DRInterpreterBuilder >> interpreterClass [

	^ DRBasicInterpreter
]

{ #category : #accessing }
DRInterpreterBuilder >> primitiveTable [
	" Answer a <Collection> of tuples representing primitive methods metadata "

	^ primitiveTable
		ifNil: [ primitiveTable := self interpreterClass basicPrimitiveTable ]
]

{ #category : #private }
DRInterpreterBuilder >> primitives: aCollection [

	primitiveTable := aCollection
]

{ #category : #accessing }
DRInterpreterBuilder >> primitivesCount [
	"Answer a <Number> specifying how many primitive methods implements the receiver. Note that primitive table entry with 0 is ignored"

	| primTableSize |

	primTableSize := self primitiveTable size.
	(self primitiveTable includes: 0)
		ifTrue: [ ^ primTableSize - 1 ].
	^ primTableSize
]
