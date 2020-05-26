Class {
	#name : #DRPhisicalRegister,
	#superclass : #DRValue,
	#instVars : [
		'name'
	],
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DRPhisicalRegister class >> basePointer [

	^ self new
		name: #BasePointer;
		yourself
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
