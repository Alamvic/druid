Class {
	#name : #DRStrongCopy,
	#superclass : #DRCopy,
	#category : #'Druid-IR'
}

{ #category : #converting }
DRStrongCopy >> asCopy [

	^ DRCopy operands: self operands copy result: self result
]

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
