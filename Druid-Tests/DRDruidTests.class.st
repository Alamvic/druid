Class {
	#name : #DRDruidTests,
	#superclass : #VMJittedPrimitivesTest,
	#category : #'Druid-Tests'
}

{ #category : #private }
DRDruidTests class >> isAbstract [

	^ self == DRDruidTests
]

{ #category : #tests }
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

{ #category : #running }
DRDruidTests >> setUp [

	super setUp.

	cogit methodZone
		setInterpreter: interpreter
		objectRepresentation: cogit objectRepresentation
		cogit: cogit.
		
	initialAddress := #invalid "will be set by test"
]
