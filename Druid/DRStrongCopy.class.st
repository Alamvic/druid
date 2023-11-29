Class {
	#name : #DRStrongCopy,
	#superclass : #DRCopy,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRStrongCopy >> isMandatoryInstruction [

	^ self shouldKeepInCFG
]

{ #category : #testing }
DRStrongCopy >> shouldKeepInCFG [

	^ self controlFlowGraph messageSends isNotEmpty
]

{ #category : #SCCP }
DRStrongCopy >> supportConstantOperand [

	^ self shouldKeepInCFG not
]
