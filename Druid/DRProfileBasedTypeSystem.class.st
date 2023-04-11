Class {
	#name : #DRProfileBasedTypeSystem,
	#superclass : #DRTypeSystem,
	#instVars : [
		'profileTable'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #adding }
DRProfileBasedTypeSystem >> addReceiver: anObject for: aRBMessageNode [

	| type |
	type := DRClassType for: anObject class.

	profileTable
		at: aRBMessageNode receiver
		ifPresent: [ :col | col add: type ]
		ifAbsentPut: [ { type } asOrderedCollection ]
]

{ #category : #initialization }
DRProfileBasedTypeSystem >> initialize [

	profileTable := Dictionary new
]

{ #category : #API }
DRProfileBasedTypeSystem >> receiverTypes: aDRMessageSend [

	| profiledTypes |
	profiledTypes := profileTable
		                 at: aDRMessageSend originAST receiver
		                 ifAbsent: [ {  } ].

	^ profiledTypes , { DRUndefinedType new } "Always ready for new types"
]
