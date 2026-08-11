Class {
	#name : 'DRFrameReturn',
	#superclass : 'DRStrongCopyForInlining',
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'testing' }
DRFrameReturn >> isCopy [

	^ self shouldKeepInCFG not
]

{ #category : 'testing' }
DRFrameReturn >> isFrameReturn [

	^ true
]

{ #category : 'SCCP' }
DRFrameReturn >> sccpLatticeValueFor: sccp [

	self shouldKeepInCFG ifTrue: [ ^ sccp bottom ].

	^ super sccpLatticeValueFor: sccp
]
