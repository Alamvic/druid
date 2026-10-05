Class {
	#name : 'DRLoadReceiver',
	#superclass : 'DRInstruction',
	#instVars : [
		'scope'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'visiting' }
DRLoadReceiver >> acceptVisitor: aVisitor [

	^ aVisitor visitLoadReceiver: self
]

{ #category : 'testing' }
DRLoadReceiver >> isLoadReceiver [

	^ true
]

{ #category : 'SCCP' }
DRLoadReceiver >> sccpLatticeValueFor: sccp [

	"We know that we know nothing about the frame pointer"
	^ sccp bottom
]

{ #category : 'types' }
DRLoadReceiver >> typeWithAlreadySeen: aCollection [ 

	"self is of the type of the class where it is declared.
	That is, it is an instance of that class or one of its subclasses"
		
	^ self operand1 scope node methodNode methodClass asDRType
]
