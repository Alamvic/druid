Class {
	#name : #DRCompare,
	#superclass : #DRTwoArgumentsInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #testing }
DRCompare >> canBeConditionalized [
	
	^ true
]

{ #category : #printing }
DRCompare >> mnemonic [

	^ 'CMP'
]
