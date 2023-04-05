"
This class provides behavior for subclasses to use a Mock JIT class.

"
Class {
	#name : #DRAbstractCompilerCollectorTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment',
		'interpreterBuilder',
		'interpreterCompiler'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #accessing }
DRAbstractCompilerCollectorTest >> interpreterWith: aCollection [

	^ self newEmptyInterpreter primitives: aCollection
]

{ #category : #accessing }
DRAbstractCompilerCollectorTest >> newEmptyInterpreter [

	^ DRDummyInterpreter new
]

{ #category : #running }
DRAbstractCompilerCollectorTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
	interpreterBuilder := DRInterpreterBuilder new.
]

{ #category : #running }
DRAbstractCompilerCollectorTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass.
	super tearDown
]

{ #category : #initialization }
DRAbstractCompilerCollectorTest >> testingJITCompilerClass [

	^ #MyJITCompilerClass
]
