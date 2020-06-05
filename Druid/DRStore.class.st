Class {
	#name : #DRStore,
	#superclass : #DRTwoArgumentsInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #visiting }
DRStore >> accept: aMCTranslator [ 

	aMCTranslator visitAdd: self
]

{ #category : #accessing }
DRStore >> destination: aDestination [

	self rightOperand: aDestination
]

{ #category : #printing }
DRStore >> mnemonic [
	
	^ 'ADD'
]

{ #category : #accessing }
DRStore >> origin: anOrigin [

	self leftOperand: anOrigin
]
