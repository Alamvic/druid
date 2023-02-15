Class {
	#name : #DRInterpreterBuilder,
	#superclass : #Object,
	#instVars : [
		'primitiveTable',
		'targetInterpreter',
		'interpreterClass'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #'instance creation' }
DRInterpreterBuilder >> basicNew [

	^ self interpreterClass basicNew
]

{ #category : #'instance creation' }
DRInterpreterBuilder >> defaultInterpreterClass [
	"Answer a default <StackInterpreter> class containing a method with the interpreter primitives "

	^ self interpreterClassArithmeticPrimitives
]

{ #category : #initialization }
DRInterpreterBuilder >> initializeWithOptions: aCollection objectMemoryClass: aClass [ 


]

{ #category : #'instance creation' }
DRInterpreterBuilder >> interpreterClass [
	" Answer the receiver's <StackInterpreter> class containing a #basicPrimitiveTable method with the interpreter primitives "

	^ interpreterClass
		ifNil: [ interpreterClass := self defaultInterpreterClass ]
]

{ #category : #'accessing - interpreters' }
DRInterpreterBuilder >> interpreterClassArithmeticPrimitives [

	^ DRBasicCogInterpreterArithmeticPrimitives
]

{ #category : #'accessing - interpreters' }
DRInterpreterBuilder >> interpreterClassObjectMemoryPrimitives [

	^ DRBasicCogInterpreterObjectMemoryPrimitives
]

{ #category : #'accessing - interpreters' }
DRInterpreterBuilder >> interpreterClassUnaryPrimitives [

	^ DRBasicCogInterpreterPrimitivesWithoutParameters
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
