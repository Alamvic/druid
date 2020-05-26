Class {
	#name : #Druid,
	#superclass : #Object,
	#category : #'Druid-Base'
}

{ #category : #generator }
Druid class >> generateIRFromBytecode: bytecodeList [
	
	^ self new generateIRFromBytecode: bytecodeList
]

{ #category : #building }
Druid >> generateIRFromBytecode: bytecodeList [

	| vmInterpreter |
	vmInterpreter := self newBytecodeInterpreter.

	^ DRASTInterpreter new
		vmInterpreter: vmInterpreter;
		interpretBytecode: bytecodeList
]

{ #category : #building }
Druid >> newBytecodeInterpreter [

	| theMemory |
	theMemory := Spur64BitCoMemoryManager simulatorClass new.

	CogVMSimulatorLSB
		initializeWithOptions: {
			#BytesPerWord. 8.
			#ObjectMemory. #Spur64BitCoMemoryManager.
			#bytecodeTableInitializer. #initializeBytecodeTableForSistaV1}
		objectMemoryClass: Spur64BitCoMemoryManager.
		
		
	^ CogVMSimulatorLSB basicNew
		objectMemory: theMemory;
		basicInitialize;
		yourself
]
