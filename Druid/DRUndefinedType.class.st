Class {
	#name : 'DRUndefinedType',
	#superclass : 'DRTopType',
	#category : 'Druid-IR-Types',
	#package : 'Druid',
	#tag : 'IR-Types'
}

{ #category : 'lookup' }
DRUndefinedType >> astForSelector: aSelector [

	^ nil
]

{ #category : 'accessing' }
DRUndefinedType >> isUndefined [

	^ true
]

{ #category : 'accessing' }
DRUndefinedType >> typeName [

	^ 'Undefined'
]
