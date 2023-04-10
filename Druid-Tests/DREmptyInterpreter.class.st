"
It represents an empty interpreter, i.e. it doesn't provide any primitive methods in its `basicPrimitiveTable` 

"
Class {
	#name : #DREmptyInterpreter,
	#superclass : #CogVMSimulatorLSB,
	#category : #'Druid-Tests'
}

{ #category : #constants }
DREmptyInterpreter class >> basicPrimitiveTable [ 

	^ Array empty
]

{ #category : #constants }
DREmptyInterpreter class >> bytecodeTableArray [ 

	^ Array empty
]

{ #category : #initialization }
DREmptyInterpreter class >> initializeBytecodeTableForSistaV1 [
	"See e.g. the cass comment for EncoderForSistaV1"
	"StackInterpreter initializeBytecodeTableForSistaV1"
	"Note: This table will be used to generate a C switch statement."

	InitializationOptions at: #SistaV1BytecodeSet put: (SistaV1BytecodeSet := true).
	BytecodeTable := Array new.

]

{ #category : #'class initialization' }
DREmptyInterpreter class >> primitiveTableArray [

	<generated>
	^ { { 0. 0 } }
]
