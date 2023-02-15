"
This class provides behavior for subclasses to use a Mock JIT class.

"
Class {
	#name : #DRAbstractMockTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment',
		'interpreterBuilder'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #accessing }
DRAbstractMockTest >> interpreterWith: aCollection [

	^ self newEmptyInterpreter primitives: aCollection
]

{ #category : #accessing }
DRAbstractMockTest >> newEmptyInterpreter [

	^ DRDummyInterpreter new
]

{ #category : #running }
DRAbstractMockTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
	interpreterBuilder := DRInterpreterBuilder new.
]

{ #category : #running }
DRAbstractMockTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass.
	super tearDown
]

{ #category : #initialization }
DRAbstractMockTest >> testingJITCompilerClass [

	^ #MyJITCompilerClass
]
