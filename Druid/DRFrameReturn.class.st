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

	^ true
]

{ #category : #SCCP }
DRFrameReturn >> sccpLatticeValueFor: sccp [

	self controlFlowGraph messageSends ifEmpty: [ "If there is no remaining messages then can be folded" 
		^ super sccpLatticeValueFor: sccp ].
	^ sccp bottom
]
