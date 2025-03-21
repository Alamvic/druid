Class {
	#name : 'DRTypeSystem',
	#superclass : 'Object',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'API' }
DRTypeSystem >> methodsForMessage: aDRMessageSend [ 

	| receiverTypes |
	receiverTypes := self receiverTypes: aDRMessageSend.
	^ receiverTypes collect: [ :type |
		  type -> (type astForSelector: aDRMessageSend selector) ]
]

{ #category : 'API' }
DRTypeSystem >> receiverTypes: aDRMessageSend [

	^ self subclassResponsibility
]

{ #category : 'private' }
DRTypeSystem >> typeOf: aRBVariableNode [ 
	
	self subclassResponsibility
]
