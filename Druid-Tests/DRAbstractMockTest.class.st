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

	^ testingEnvironment 
		at: self jitCompilerClassNameForTest 
		ifAbsent: [ 
			Smalltalk image classInstaller make: [ : builder |
				builder
					name: self jitCompilerClassNameForTest;
					superclass: self jitSuperclassForTest;
					category: self jitCompilerClassNameForTest ]. ]


]

{ #category : #running }
DRAbstractMockTest >> jitCompilerClassNameForTest [ 

	^ #MyJITCompilerClass
]

{ #category : #running }
DRAbstractMockTest >> jitSuperclassForTest [

	^ CogVMSimulatorLSB
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
