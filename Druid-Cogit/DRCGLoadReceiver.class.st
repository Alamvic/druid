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

	"self is of the type of the class where it is declared.
	That is, it is an instance of that class or one of its subclasses"
		
	^ self operand1 scope node methodNode methodClass asDRType
]
