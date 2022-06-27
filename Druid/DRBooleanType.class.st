Class {
	#name : #DRBooleanType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #converting }
DRBooleanType >> and: aBlock [

	self ifFalse: [ ^ false ].
	^ aBlock value
]

{ #category : #converting }
DRBooleanType >> asInteger [

	^ self = true
		ifTrue: [ 1 ]
		ifFalse: [ 0 ]
]

{ #category : #converting }
DRBooleanType >> ifFalse: falseBlock [

	self ifTrue: [ ^ nil ].
	^ falseBlock value
]

{ #category : #converting }
DRBooleanType >> ifFalse: falseBlock ifTrue: trueBlock [

	self ifTrue: [ ^ trueBlock value ].
	^ falseBlock value
]

{ #category : #converting }
DRBooleanType >> ifTrue: trueBlock ifFalse: falseBlock [

	self ifTrue: [ ^ trueBlock value ].
	^ falseBlock value
]

{ #category : #converting }
DRBooleanType >> not [

	self ifTrue: [ ^ false ].
	^ true
]

{ #category : #converting }
DRBooleanType >> or: aBlock [

	self ifTrue: [ ^ self ].
	^ aBlock value
]
