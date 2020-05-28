Class {
	#name : #DRRegister,
	#superclass : #DRValue,
	#instVars : [
		'name'
	],
	#category : #'Druid-Instructions'
}

{ #category : #'instance creation' }
DRRegister class >> named: aName [

	^ self new
		name: aName;
		yourself
]

{ #category : #comparing }
DRRegister >> = aRegister [

	(aRegister isKindOf: self species)
		ifFalse: [ ^ false ].
	^ aRegister name = name
]

{ #category : #comparing }
DRRegister >> hash [

	^ name hash
]

{ #category : #testing }
DRRegister >> isConstant [
	
	^ false
]

{ #category : #testing }
DRRegister >> isRegister [
	
	^ true
]

{ #category : #accessing }
DRRegister >> name [
	^ name
]

{ #category : #accessing }
DRRegister >> name: anObject [
	name := anObject
]

{ #category : #accessing }
DRRegister >> printOn: aStream [

	^ aStream nextPutAll: name
]

{ #category : #accessing }
DRRegister >> result [
	
	^ self
]
