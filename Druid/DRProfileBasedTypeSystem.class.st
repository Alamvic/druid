Class {
	#name : #DRProfileBasedTypeSystem,
	#superclass : #DRTypeSystem,
	#instVars : [
		'profileTable'
	],
	#classInstVars : [
		'default'
	],
	#category : #'Druid-IR-Types'
}

{ #category : #accessing }
DRProfileBasedTypeSystem class >> default [

	^ default ifNil: [ self reset ]
]

{ #category : #accessing }
DRProfileBasedTypeSystem class >> reset [

	^ default := self new
]

{ #category : #adding }
DRProfileBasedTypeSystem >> addReceiverInfo: anObject forNode: aRBNode [

	| type |
	type := DRClassType for: anObject class.

	profileTable
		at: aRBNode
		ifPresent: [ :col | col add: type ]
		ifAbsentPut: [ { type } asOrderedCollection ]
]

{ #category : #adding }
DRProfileBasedTypeSystem >> addReceiverInfo: anObject inMessage: aRBMessageNode [

	self addReceiverInfo: anObject forNode: aRBMessageNode receiver
]

{ #category : #initialization }
DRProfileBasedTypeSystem >> initialize [

	profileTable := Dictionary new
]

{ #category : #accessing }
DRProfileBasedTypeSystem >> profileTable [

	^ profileTable
]

{ #category : #API }
DRProfileBasedTypeSystem >> receiverTypes: aDRMessageSend [

	| profiledTypes |
	profiledTypes := profileTable
		                 at: aDRMessageSend originAST receiver
		                 ifAbsent: [ {  } ].

	^ profiledTypes , { DRUndefinedType new } "Always ready for new types"
]
