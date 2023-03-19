"
This class provides behavior for subclasses to use a Mock JIT class.

"
Class {
	#name : #DRAbstractMockTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment',
		'interpreterBuilder',
		'interpreterCompiler'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #accessing }
DRAbstractMockTest >> interpreterWith: aCollection [

	^ interpreterBuilder primitives: aCollection
]

{ #category : #running }
DRAbstractMockTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
	interpreterBuilder := DRInterpreterBuilder new
		targetSuperclass: DRBasicCogInterpreterArithmeticPrimitives;
		yourself
]

{ #category : #running }
DRAbstractMockTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass name.
	super tearDown
]

{ #category : #initialization }
DRAbstractMockTest >> testingJITCompilerClass [

	^ #MyJITCompilerClass
]
