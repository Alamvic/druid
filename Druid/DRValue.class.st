Class {
	#name : #DRValue,
	#superclass : #DRInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #'as yet unclassified' }
DRValue >> appendOn: anInstructionBuilder [

	"Do nothing. Registers are not appended. Only on access"
]

{ #category : #'as yet unclassified' }
DRValue >> printOn: aStream [

	^ aStream nextPutAll: self result asString
]
