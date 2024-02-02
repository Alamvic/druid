Class {
	#name : #DRFromTemporary,
	#superclass : #Object,
	#instVars : [
		'name',
		'index'
	],
	#category : #'Druid-BytecodeGeneration-Deoptimization'
}

{ #category : #accessing }
DRFromTemporary >> name [
	^ name
]

{ #category : #accessing }
DRFromTemporary >> name: aString [ 
	name := aString
]

{ #category : #reading }
DRFromTemporary >> readFromContext: aContext [ 
	
	^ aContext tempAt: index
]

{ #category : #updating }
DRFromTemporary >> updateInformationFrom: aDRBytecodeGenerator [ 
	
	index := aDRBytecodeGenerator ir tempMap at: name
]
