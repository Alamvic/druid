Class {
	#name : 'DRUnknownTypeSystem',
	#superclass : 'DRTypeSystem',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'API' }
DRUnknownTypeSystem >> receiverTypes: aDRMessageSend [

	^ { DRUndefinedType new }
]
