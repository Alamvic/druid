Class {
	#name : #DRCogitSendMarshallInsertion,
	#superclass : #DROptimisation,
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #accessing }
DRCogitSendMarshallInsertion >> applyTo: cfg [

	cfg instructionsDo: [ :e |
		e isJITMessageSend ifTrue: [ e addBefore: (DRCogitSendMarshall send: e) ] ]
]
