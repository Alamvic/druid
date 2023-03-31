"
This class provides behavior for subclasses to use a Mock JIT class.

"
Class {
	#name : #DRAbstractMockTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #initialization }
DRAbstractMockTest >> jitCompilerClassForTest [

	^ #MyJITCompilerClass
]

{ #category : #running }
DRAbstractMockTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
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
