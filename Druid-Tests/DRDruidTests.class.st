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
DRDruidTests class >> testParameters [

	"Run only 64 bit tests for now..."
	^ ParametrizedTestMatrix new
		addCase: { #ISA -> #'aarch64'. #wordSize -> 8};
		addCase: { #ISA -> #'X64'. #wordSize -> 8};
		yourself
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
