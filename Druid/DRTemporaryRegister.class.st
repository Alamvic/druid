Class {
	#name : #DRTemporaryRegister,
	#superclass : #DRValue,
	#instVars : [
		'id'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance-creation' }
DRTemporaryRegister class >> id: anInteger [ 
	
	^ self new
		id: anInteger;
		yourself
]

{ #category : #accessing }
DRTemporaryRegister >> = aRegister [

	(self isKindOf: aRegister species)
		ifFalse: [ ^ false ].
	^ self id = aRegister id
]

{ #category : #accessing }
DRTemporaryRegister >> hash [

	^ id hash
]

{ #category : #accessing }
DRTemporaryRegister >> id [
	^ id
]

{ #category : #accessing }
DRTemporaryRegister >> id: anInteger [ 
	id := anInteger
]

{ #category : #testing }
DRTemporaryRegister >> isRegister [

	^ true
]

{ #category : #accessing }
DRTemporaryRegister >> isVirtualRegister [
	
	^ true
]

{ #category : #accessing }
DRTemporaryRegister >> name [

	^ 'R' , self id asString
]
