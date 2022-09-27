Class {
	#name : #DRUndefinedType,
	#superclass : #DRTopType,
	#category : #'Druid-IR-Types'
}

{ #category : #lookup }
DRUndefinedType >> astForSelector: aSelector [

	^ nil
]

{ #category : #lookup }
DRUndefinedType >> lookupSelector: aString [

	^ nil
]

{ #category : #accessing }
DRUndefinedType >> typeName [

	^ 'Undefined'
]
