Class {
	#name : #DRFromReceiver,
	#superclass : #Object,
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #reading }
DRFromReceiver >> readFromContext: aContext [ 
	
	^ aContext receiver
]

{ #category : #updating }
DRFromReceiver >> updateInformationFrom: aDRBytecodeGenerator [ 

	"Nothing to do"
]
