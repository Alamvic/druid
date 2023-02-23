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
