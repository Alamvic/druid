Class {
	#name : #DRGeneralPrimitiveTest,
	#superclass : #VMJittedGeneralPrimitiveTest,
	#category : #'Druid-Quarantine'
}

{ #category : #running }
DRGeneralPrimitiveTest >> argumentsOnRegisters [

	"what our compiler does..."
	^ true
]

{ #category : #running }
DRGeneralPrimitiveTest >> interpreterClass [

	^ CogVMSimulatorLSB
]

{ #category : #running }
DRGeneralPrimitiveTest >> jitCompilerClass [

	^ DruidCogit
]

{ #category : #running }
DRGeneralPrimitiveTest >> jitOptions [

	^ super jitOptions
		  at: #bytecodeTableInitializer put: #initializeBytecodeTableForSistaV1;
		  yourself
]

{ #category : #helpers }
DRGeneralPrimitiveTest >> setUp [

	super setUp.
	
	cogit objectRepresentation: cogit.
	cogit methodZone
		setInterpreter: interpreter
		objectRepresentation: cogit
		cogit: cogit
]
