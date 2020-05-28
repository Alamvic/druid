Class {
	#name : #DRSimulateGeneratedCodeTest,
	#superclass : #VMSimpleStackBasedCogitBytecodeTest,
	#instVars : [
		'builder',
		'mcTranslator',
		'address',
		'endAddress'
	],
	#pools : [
		'CogRTLOpcodes'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSimulateGeneratedCodeTest >> buildIRAndRun: aBlock [ 

	builder := DRIRBuilder new.
	builder isa: isa.

	aBlock value.
	builder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator
		translate: builder instructions
		withCompiler: cogit.

	address := cogit methodZone freeStart.
	endAddress := mcTranslator generate.

	self runFrom: address until: endAddress.	

]

{ #category : #tests }
DRSimulateGeneratedCodeTest >> testAddingSomeConstantAndPushingThemPutThemIntheStack [

	self buildIRAndRun: [ 
		builder push: (builder add: (builder newConstant: 17) and: (builder newConstant: 25)) ].

	self assert: self popAddress equals: 42
]

{ #category : #tests }
DRSimulateGeneratedCodeTest >> testPushConstantInstructionDoesAPush [

	self buildIRAndRun: [ 
		builder push: (DRConstant value: 17) ].
	
	self assert: self popAddress equals: 17
]
