Class {
	#name : #DRCustomisationTypeSystem,
	#superclass : #DRTypeSystem,
	#category : #'Druid-IR-Types'
}

{ #category : #API }
DRCustomisationTypeSystem >> receiverTypes: aDRMessageSend [ 

		^ { aDRMessageSend receiver type }
]
