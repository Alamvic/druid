Class {
	#name : #DRFrameReturn,
	#superclass : #DRStrongCopy,
	#category : #'Druid-IR'
}

{ #category : #testing }
DRFrameReturn >> isCopy [

	^ self shouldKeepInCFG not
]

{ #category : #testing }
DRFrameReturn >> isFrameReturn [

	^ true
]

{ #category : #SCCP }
DRFrameReturn >> sccpLatticeValueFor: sccp [

	self shouldKeepInCFG ifTrue: [ ^ sccp bottom ].

	^ super sccpLatticeValueFor: sccp
]
