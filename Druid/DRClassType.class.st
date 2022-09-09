Class {
	#name : #DRClassType,
	#superclass : #DRTopType,
	#instVars : [
		'classType'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #'instance creation' }
DRClassType class >> for: aClass [

	| type |
	type := self new.
	type classType: aClass.
	^ type
]

{ #category : #'instance creation' }
DRClassType >> classType: aClass [

	classType := aClass
]

{ #category : #'instance creation' }
DRClassType >> lookupClass [
	
	^ classType
]
