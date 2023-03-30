Class {
	#name : #DRUnknownTypeSystem,
	#superclass : #DRTypeSystem,
	#category : #'Druid-IR-Types'
}

{ #category : #API }
DRUnknownTypeSystem >> receiverTypes: aDRMessageSend [

	^ { DRUndefinedType new }
]
