Class {
	#name : #DRDruidTest,
	#superclass : #VMJittedPrimitivesTest,
	#instVars : [
		'fakeTrampoline'
	],
	#category : #'Druid-Tests'
}

{ #category : #private }
DRDruidTest class >> isAbstract [

	^ self == DRDruidTest
]

{ #category : #'building suites' }
DRDruidTest class >> wordSizeParameters [

	^ self wordSize64Parameters
]

{ #category : #'helpers-compiling' }
DRDruidTest >> generatorSelector: anInterpreterSelector [

	^ ('gen_' , anInterpreterSelector) asSymbol
]

{ #category : #'helpers-compiling' }
DRDruidTest >> interpreterClass [ 

	^ DruidTestInterpreter
]

{ #category : #'helpers-compiling' }
DRDruidTest >> jitCompilerClass [

	^ DruidTestRTLCompiler
]

{ #category : #'helpers-compiling' }
DRDruidTest >> jitOptions [

	^ super jitOptions
		  at: #IMMUTABILITY put: true;
		  yourself
]

{ #category : #helpers }
DRDruidTest >> setUp [

	super setUp.

	cogit methodZone
		setInterpreter: interpreter
		objectRepresentation: cogit objectRepresentation
		cogit: cogit.
		
	fakeTrampoline := self compile: [ cogit RetN: 0 ].
		
	cogInitialAddress := #invalid "will be set by test"
]
