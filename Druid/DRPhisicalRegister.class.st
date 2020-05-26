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

{ #category : #comparing }
DRPhisicalRegister >> = aRegister [

	(aRegister isKindOf: self species)
		ifFalse: [ ^ false ].
	^ aRegister name = name
]

{ #category : #comparing }
DRPhisicalRegister >> hash [

	^ name hash
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
