Class {
	#name : #DRAdd,
	#superclass : #DRTwoArgumentsInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #visiting }
DRAdd >> accept: aMCTranslator [ 

	aMCTranslator visitAdd: self
]

{ #category : #printing }
DRAdd >> mnemonic [
	
	^ 'ADD'
]
