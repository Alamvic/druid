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

{ #category : #comparing }
DRClassType >> = aType [

	^ (self species = aType species) and: [ classType = aType classType ]
]

{ #category : #lookup }
DRClassType >> astForSelector: aSelector [ 
	^ (classType lookupSelector: aSelector) ast
]

{ #category : #accessing }
DRClassType >> classType [

	^ classType
]

{ #category : #accessing }
DRClassType >> classType: aClass [

	classType := aClass
]

{ #category : #comparing }
DRClassType >> hash [

	^ self species hash bitXor: classType hash
]

{ #category : #'instance creation' }
DRClassType >> lookupClass [
	
	^ classType
]
