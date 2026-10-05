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
