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
	aDRMessageSend originAST isSuperSend ifTrue: [
		^ { (self superMethodTupleForMessage: aDRMessageSend) } ].

	receiverTypes := self receiverTypes: aDRMessageSend.
	^ receiverTypes
		  collect: [ :type |
		  type -> (type astForSelector: aDRMessageSend selector) ]
		  thenReject: [ :assoc |
			  assoc value
				  ifNotNil: [ :m | self shouldIgnore: m ]
				  ifNil: [ false ] ]
]

{ #category : 'API' }
DRTypeSystem >> receiverTypes: aDRMessageSend [

	^ self subclassResponsibility
]

{ #category : 'asserting' }
DRTypeSystem >> shouldIgnore: aMethodNode [

	^ aMethodNode isPrimitive or: [
		aMethodNode sourceCode includesSubstring: #subclassResponsibility ]
]

{ #category : 'as yet unclassified' }
DRTypeSystem >> superMethodTupleForMessage: aDRMessageSend [

	^ aDRMessageSend receiver type
	  -> (aDRMessageSend originAST methodNode methodClass superclass
			   lookupSelector: aDRMessageSend selector) ast
]

{ #category : 'private' }
DRTypeSystem >> typeOf: aRBVariableNode [ 
	
	self subclassResponsibility
]
