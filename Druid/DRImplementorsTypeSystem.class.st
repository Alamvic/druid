Class {
	#name : #DRImplementorsTypeSystem,
	#superclass : #DRTypeSystem,
	#category : #'Druid-IR-Types'
}

{ #category : #API }
DRImplementorsTypeSystem >> receiverTypes: aDRMessageSend [

	^ aDRMessageSend selector implementors collect: [ :i | DRClassType for: i methodClass ]
]
