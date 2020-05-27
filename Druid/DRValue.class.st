Class {
	#name : #DRValue,
	#superclass : #DRInstruction,
	#category : #'Druid-Instructions'
}

{ #category : #'as yet unclassified' }
DRValue >> appendOn: anInstructionBuilder [

	"Do nothing. Registers are not appended. Only on access"
]

{ #category : #printing }
DRValue >> result [

	^ self
]
