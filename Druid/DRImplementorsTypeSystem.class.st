Class {
	#name : 'DRImplementorsTypeSystem',
	#superclass : 'DRTypeSystem',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'API' }
DRImplementorsTypeSystem >> receiverTypes: aDRMessageSend [

	^ aDRMessageSend selector implementors collect: [ :i | DRClassType for: i methodClass ]
]
