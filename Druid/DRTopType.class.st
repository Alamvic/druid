Class {
	#name : #DRTopType,
	#superclass : #Integer,
	#category : #'Druid-IR-Types'
}

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

	self = nil ifFalse: [ aBlock cull: self ].
	^ nil
]

{ #category : #testing }
DRTopType >> ifNotNil: notNilBlock ifNil: nilBlock [

	self = nil ifTrue: [ ^ nilBlock value ].
	^ notNilBlock cull: self
]

{ #category : #lookup }
DRTopType >> lookupClass [
	
	^ self class
]
