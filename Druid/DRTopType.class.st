Class {
	#name : #DRTopType,
	#superclass : #Object,
	#category : #'Druid-IR-Types'
}

{ #category : #testing }
DRTopType >> ifNil: aBlock [

	self = nil ifTrue: [ ^ aBlock value ]
]

{ #category : #testing }
DRTopType >> ifNil: nilBlock ifNotNil: notNilBlock [

	self = nil ifTrue: [ ^ nilBlock value ].
	^ notNilBlock value
]

{ #category : #testing }
DRTopType >> ifNotNil: aBlock [

	self = nil ifFalse: [ aBlock value ].
	^ nil
]

{ #category : #testing }
DRTopType >> ifNotNil: notNilBlock ifNil: nilBlock [

	self = nil ifTrue: [ ^ nilBlock value ].
	^ notNilBlock value
]

{ #category : #lookup }
DRTopType >> lookupClass [
	
	^ self class
]
