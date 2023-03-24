Class {
	#name : #DRTypeSystem,
	#superclass : #Object,
	#category : #'Druid-IR-Types'
}

{ #category : #API }
DRTypeSystem >> methodsForMessage: aDRMessageSend [

	^ self subclassResponsibility
]
