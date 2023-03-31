Class {
	#name : #DRInterpreterBuilderToCompiler,
	#superclass : #DRInterpreterToCompiler,
	#instVars : [
		'interpreter',
		'interpreterBuilder'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #initialization }
DRInterpreterBuilderToCompiler >> initialize [

	super initialize.
	interpreter := self newInterpreter
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreterBuilder [

	^ interpreterBuilder
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreterBuilder: aClass [ 

	interpreterBuilder := aClass
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^  interpreterBuilder primitiveTable
]

{ #category : #accessing }
DRInterpreterBuilderToCompiler >> newInterpreter [
	" Answer an <AbstractInterpreter|StackInterpreter> from the receiver's interpreter builder "
	memory := self newMemory.

	interpreterBuilder 
		initializeWithOptions: self initializationOptions
		objectMemoryClass: memory class.

	^ interpreterBuilder basicNew
		  objectMemory: memory;
		  basicInitialize
]
