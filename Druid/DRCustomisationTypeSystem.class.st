Class {
	#name : 'DRCustomisationTypeSystem',
	#superclass : 'DRTypeSystem',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'API' }
DRCustomisationTypeSystem >> receiverTypes: aDRMessageSend [

	aDRMessageSend receiver type isUndefined ifTrue: [
		({ #ifFalse:. #ifTrue:. #ifTrue:ifFalse: } includes: aDRMessageSend selector) ifTrue: [
			^ { DRBooleanType new } ].
		^ {  } ].

	^ { aDRMessageSend receiver type } reject: [ :type |
		  (type astForSelector: aDRMessageSend selector) isPrimitive ]
]
