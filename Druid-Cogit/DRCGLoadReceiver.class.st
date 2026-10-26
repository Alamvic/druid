Class {
	#name : 'DRCGLoadReceiver',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-Cogit-IR',
	#package : 'Druid-Cogit',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRCGLoadReceiver >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadReceiver: self
]

{ #category : 'testing' }
DRCGLoadReceiver >> isLoadReceiver [

	^ true
]

{ #category : 'SCCP' }
DRCGLoadReceiver >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : 'types' }
DRCGLoadReceiver >> typeWithAlreadySeen: aCollection [ 

	^ DRSignedIntegerType bits64
]
