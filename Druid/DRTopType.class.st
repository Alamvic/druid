Class {
	#name : #DRTopType,
	#superclass : #Object,
	#category : #'Druid-IR-Types'
}

{ #category : #comparing }
DRTopType >> = aType [

	^ self species = aType species
]

{ #category : #lookup }
DRTopType >> astForSelector: aSelector [

	^ (self lookupSelector: aSelector) ast
]

{ #category : #comparing }
DRTopType >> hash [

	^ self species hash
]

{ #category : #testing }
DRTopType >> ifNil: aBlock [

	self = nil ifTrue: [ ^ aBlock value ]
]

{ #category : #testing }
DRTopType >> ifNil: nilBlock ifNotNil: notNilBlock [

	self = nil ifTrue: [ ^ nilBlock value ].
	^ notNilBlock  cull: self
]

{ #category : #testing }
DRTopType >> ifNotNil: aBlock [

	self = nil ifFalse: [ ^ aBlock cull: self ].
	^ nil
]

{ #category : #testing }
DRTopType >> ifNotNil: notNilBlock ifNil: nilBlock [

	self = nil ifTrue: [ ^ nilBlock value ].
	^ notNilBlock cull: self
]

{ #category : #testing }
DRTopType >> isFloatType [
	
	^ false
]

{ #category : #testing }
DRTopType >> isUndefined [

	^ false
]

{ #category : #lookup }
DRTopType >> lookupClass [
	
	^ self class
]

{ #category : #lookup }
DRTopType >> lookupSelector: aString [ 

	^ (self class lookupSelector: aString)
		ifNil: [ self systemClass lookupSelector: aString ]
]

{ #category : #accessing }
DRTopType >> systemClass [

	^ nil class
]
