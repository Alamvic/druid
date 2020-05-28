Class {
	#name : #DRSimulateGeneratedBytecodeTest,
	#superclass : #VMSimpleStackBasedCogitBytecodeTest,
	#instVars : [
		'builder',
		'astInterpreter',
		'mcTranslator'
	],
	#pools : [
		'CogRTLOpcodes'
	],
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRSimulateGeneratedBytecodeTest >> newInterpreter [ 

	| theMemory |
	theMemory := self newMemory.

	CogVMSimulatorLSB
		initializeWithOptions: {
			#BytesPerWord. 8.
			#ObjectMemory. theMemory class name.
			#bytecodeTableInitializer. #initializeBytecodeTableForSistaV1}
		objectMemoryClass: theMemory class.
		
		
	^ CogVMSimulatorLSB basicNew
		objectMemory: theMemory;
		basicInitialize
]

{ #category : #helpers }
DRSimulateGeneratedBytecodeTest >> setUp [ 
	
	super setUp.
	

	builder := DRIRBuilder new.
	builder isa: isa.

	astInterpreter := DRASTInterpreter new.
	astInterpreter vmInterpreter: interpreter.
	astInterpreter irBuilder: builder.	

]

{ #category : #'tests - single bytecode' }
DRSimulateGeneratedBytecodeTest >> testPushReceiverBytecodePushesTheReceiver [

	| address endAddress |
	astInterpreter interpretBytecode: #[76].
	astInterpreter irBuilder assignPhysicalRegisters.

	mcTranslator := DRIntermediateRepresentationToMachineCodeTranslator
		translate: builder instructions
		withCompiler: cogit.

	address := cogit methodZone freeStart.
	endAddress := mcTranslator generate.

	self 
		createInterpreterFramefulCallFrom: callerAddress 
		receiver: memory trueObject arguments: #() temporaries: #().

	self runFrom: address until: endAddress.

	self assert: (self popAddress) equals: memory trueObject.
	
]
