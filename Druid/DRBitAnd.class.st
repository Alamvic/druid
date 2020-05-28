Class {
	#name : #DRBitAnd,
	#superclass : #DRTwoArgumentsInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #testing }
DRBitAnd >> canBeConditionalized [
	
	^ true
]

{ #category : #printing }
DRBitAnd >> mnemonic [
	
	^ 'AND'
]
