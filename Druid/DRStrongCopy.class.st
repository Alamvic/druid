Class {
	#name : #DRStrongCopy,
	#superclass : #DRCopy,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRStrongCopy >> isCopy [

	^ self shouldKeepInCFG not
]

{ #category : #testing }
DRStrongCopy >> isMandatoryInstruction [

	^ self shouldKeepInCFG
]

{ #category : #SCCP }
DRStrongCopy >> sccpLatticeValueFor: sccp [

	self shouldKeepInCFG ifTrue: [ ^ sccp bottom ].

	^ super sccpLatticeValueFor: sccp
]

{ #category : #testing }
DRStrongCopy >> shouldKeepInCFG [

	^ self controlFlowGraph messageSends isNotEmpty
]

{ #category : #SCCP }
DRStrongCopy >> supportConstantOperand [

	^ self shouldKeepInCFG not
]
