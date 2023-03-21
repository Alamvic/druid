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

{ #category : #initialization }
DRAbstractMockTest >> jitCompilerClassForTest [

	^ #MyJITCompilerClass
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

	| className |

	className := self jitCompilerClassForTest.
	className isSymbol
		ifFalse: [ className := self jitCompilerClassForTest name ].
	testingEnvironment removeClassNamed: className.
	super tearDown
]
