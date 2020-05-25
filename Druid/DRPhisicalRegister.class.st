Class {
	#name : #DRPhisicalRegister,
	#superclass : #DRInstruction,
	#instVars : [
		'name'
	],
	#category : #'Druid-instructions'
}

{ #category : #'instance creation' }
DRPhisicalRegister class >> basePointer [

	^ self new
		name: #BasePointer;
		yourself
]

{ #category : #'as yet unclassified' }
DRPhisicalRegister >> appendOn: anInstructionBuilder [

	"Do nothing. Registers are not appended. Only on access"
]

{ #category : #accessing }
DRPhisicalRegister >> name [
	^ name
]

{ #category : #accessing }
DRPhisicalRegister >> name: anObject [
	name := anObject
]

{ #category : #accessing }
DRPhisicalRegister >> result [
	
	^ name
]
