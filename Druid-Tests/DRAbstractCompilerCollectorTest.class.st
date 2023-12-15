"
This class provides behavior for subclasses to use a Mock JIT class.

"
Class {
	#name : #DRAbstractCompilerCollectorTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #initialization }
DRAbstractCompilerCollectorTest >> jitCompilerClassForTest [

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
DRAbstractCompilerCollectorTest >> jitCompilerClassNameForTest [ 

	^ #MyJITCompilerClass
]

{ #category : #running }
DRAbstractCompilerCollectorTest >> jitSuperclassForTest [

	^ StackToRegisterMappingCogit
]

{ #category : #running }
DRAbstractCompilerCollectorTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
]

{ #category : #running }
DRAbstractCompilerCollectorTest >> tearDown [

	| className |

	className := self jitCompilerClassForTest.
	className isSymbol
		ifFalse: [ className := self jitCompilerClassForTest name ].
	testingEnvironment removeClassNamed: className.
	super tearDown
]
