Class {
	#name : #DRDruidTests,
	#superclass : #VMJittedPrimitivesTest,
	#instVars : [
		'fakeTrampoline'
	],
	#category : #'Druid-Tests'
}

{ #category : #private }
DRDruidTests class >> isAbstract [

	^ self == DRDruidTests
]

{ #category : #'building suites' }
DRDruidTests class >> wordSizeParameters [

	^ self wordSize64Parameters
]

{ #category : #'helpers-compiling' }
DRDruidTests >> interpreterClass [ 

	^ DruidTestInterpreter
]

{ #category : #'helpers-compiling' }
DRDruidTests >> jitCompilerClass [

	^ DruidTestRTLCompiler
]

{ #category : #'helpers-compiling' }
DRDruidTests >> jitOptions [

	^ super jitOptions
		  at: #bytecodeTableInitializer put: #initializeBytecodeTableForSistaV1;
		  yourself
]

{ #category : #helpers }
DRDruidTests >> setUp [

	super setUp.

	cogit methodZone
		setInterpreter: interpreter
		objectRepresentation: cogit objectRepresentation
		cogit: cogit.
		
	fakeTrampoline := self compile: [ cogit RetN: 0 ].
		
	cogInitialAddress := #invalid "will be set by test"
]
