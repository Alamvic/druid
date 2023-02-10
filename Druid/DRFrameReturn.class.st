Class {
	#name : #DRFrameReturn,
	#superclass : #DRCopy,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRFrameReturn >> isCopy [

	^ false
]

{ #category : #testing }
DRFrameReturn >> isFrameReturn [

	^ true
]

{ #category : #testing }
DRFrameReturn >> isMandatoryInstruction [

	^ self controlFlowGraph messageSends isNotEmpty
]

{ #category : #SCCP }
DRFrameReturn >> sccpLatticeValueFor: sccp [

	self isMandatoryInstruction ifTrue: [ ^ sccp bottom ].

	^ super sccpLatticeValueFor: sccp
]

{ #category : #SCCP }
DRFrameReturn >> supportConstantOperand [

	^ self isMandatoryInstruction not
]
