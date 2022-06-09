Class {
	#name : #DRBooleanType,
	#superclass : #Object,
	#category : #'Druid-IR-Types'
}

{ #category : #converting }
DRBooleanType >> asInteger [

	^ self = true
		ifTrue: [ 1 ]
		ifFalse: [ 0 ]
]

{ #category : #interpreting }
DRBooleanType >> lookupClass [
	
	^ self class
]
