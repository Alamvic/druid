Class {
	#name : #DRDispatchTable,
	#superclass : #Object,
	#instVars : [
		'interpreterClass',
		'primitives',
		'scanSuperclasses',
		'allowDuplicates',
		'excludeObjectClass'
	],
	#category : #'Druid-DispatchTable-Model'
}

{ #category : #examples }
DRDispatchTable class >> example [

	| dispatchTable |

	dispatchTable := DRDispatchTable for: StackInterpreterPrimitives. "InterpreterPrimitives"
	dispatchTable := DRDispatchTable for: StackInterpreterPrimitives.
	dispatchTable := DRDispatchTable for: StackInterpreter.
	"dispatchTable := DRDispatchTable for: CoInterpreter." 
	dispatchTable selectorsFromPattern: 'prim'.
	dispatchTable addFromProtocol: ''.
	dispatchTable addFromProtocol: 'arithmetic float primitives'.
	dispatchTable addFromProtocols: #(
		'arithmetic float primitives' 
		'control primitives').
	" Obtain the Dictionary of primitives "
	dispatchTable primitives.
	dispatchTable generateInterpreterTableIn: #TargetClass.
]

{ #category : #'instance creation' }
DRDispatchTable class >> for: anInterpreterClass [

	^ self new
		initializeFor: anInterpreterClass;
		yourself
]

{ #category : #accessing }
DRDispatchTable >> addFromProtocol: aString [
	" Add all methods from the protocol named aString in the receiver's interpreter classs"

	aString ifEmpty: [ ^ self ].
	(self interpreterProtocol hasProtocolNamed: aString)
		ifTrue: [ self primitives addAll: (self interpreterProtocol methodsInProtocolNamed: aString) ]
]

{ #category : #accessing }
DRDispatchTable >> addFromProtocols: aCollectionOfProtocolNames [
	" Add all methods from the protocol named aString in the receiver's interpreter classs"

	aCollectionOfProtocolNames
		do: [ : protocolName | self addFromProtocol: protocolName ]
		displayingProgress: [ : name | 'Adding primitives in ' , name ]
]

{ #category : #private }
DRDispatchTable >> allMethodsMatching: aString [
	"Answer a <Collection> of <CompilerMethod> beginning with aString, looking up the pattern in the receiver's interpreter class methodDictionary, chase the superclass chain and try again until no method is found."

	| lookupClass allSelectors |
	lookupClass := self interpreterClass.
	allSelectors := self collectorClass new.
	[ self finishScanIn: lookupClass ] whileFalse: [
		allSelectors addAll: (lookupClass methods select: [ : method | method selector beginsWith: aString ]).
		lookupClass := lookupClass superclass ].
	^ allSelectors
]

{ #category : #private }
DRDispatchTable >> allSelectorsMatching: aString [
	"Answer a <Collection> of selectors begining with aString, looking up the pattern in the receiver's interpreter class methodDictionary, chase the superclass chain and try again until no method is found."

	| lookupClass allSelectors |
	lookupClass := self interpreterClass.
	allSelectors := self collectorClass new.
	[ lookupClass isNil ] whileFalse: [
		allSelectors addAll: (lookupClass selectors select: [ :selector | selector beginsWith: aString ]).
		lookupClass := lookupClass superclass ].
	^ allSelectors
]

{ #category : #accessing }
DRDispatchTable >> allowDuplicates [

	^ allowDuplicates
]

{ #category : #accessing }
DRDispatchTable >> allowDuplicates: anObject [

	allowDuplicates := anObject
]

{ #category : #accessing }
DRDispatchTable >> collectorClass [

	^ self allowDuplicates
		ifTrue: [ Bag ]
		ifFalse: [ OrderedCollection ]
]

{ #category : #private }
DRDispatchTable >> excludeObjectClass [

	^ excludeObjectClass
		ifNil: [ excludeObjectClass := true ]
]

{ #category : #private }
DRDispatchTable >> finishScanIn: lookupClass [

	^ lookupClass isNil or: [ self excludeObjectClass and: [ lookupClass = Object ] ]
]

{ #category : #accessing }
DRDispatchTable >> generateInterpreterTableIn: aClassName [

	self shouldBeImplemented
]

{ #category : #initialization }
DRDispatchTable >> initializeFor: anInterpreterClass [

	interpreterClass := anInterpreterClass.
	scanSuperclasses := true.
	allowDuplicates := true.
]

{ #category : #accessing }
DRDispatchTable >> interpreterClass [
	^ interpreterClass
]

{ #category : #accessing }
DRDispatchTable >> interpreterClass: anObject [

	interpreterClass := anObject
]

{ #category : #accessing }
DRDispatchTable >> interpreterProtocol [
	" Answer the <ProtocolOrganizer> for the receiver's interpreter "

	^ self interpreterClass organization protocolOrganizer
]

{ #category : #querying }
DRDispatchTable >> methodsFromPattern: aString [
	"Answer a <Collection> of selectors begining with aString"

	aString ifEmpty: [ ^ Array empty ].
	^ self primitives addAll: (self scanSuperclasses
		ifTrue: [ self allMethodsMatching: aString ]
		ifFalse: [ self methodsMatching: aString ])
]

{ #category : #private }
DRDispatchTable >> methodsMatching: aString [
	"Answer a <Collection> of selectors begining with aString"

	^ self interpreterClass methods
		select: [ : method | method selector beginsWith: aString ]
]

{ #category : #accessing }
DRDispatchTable >> primitives [
	"Answer a <Collection> of the primitives to be translated"
	
	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]

{ #category : #accessing }
DRDispatchTable >> primitives: anObject [

	primitives := anObject
]

{ #category : #accessing }
DRDispatchTable >> scanSuperclasses [

	^ scanSuperclasses
]

{ #category : #accessing }
DRDispatchTable >> scanSuperclasses: anObject [

	scanSuperclasses := anObject
]

{ #category : #querying }
DRDispatchTable >> selectorsFromPattern: aString [
	"Answer a <Collection> of selectors begining with aString"

	aString ifEmpty: [ ^ Array empty ].
	^ self primitives addAll: (self scanSuperclasses
		ifTrue: [ self allSelectorsMatching: aString ]
		ifFalse: [ self selectorsMatching: aString ])
]

{ #category : #private }
DRDispatchTable >> selectorsMatching: aString [
	"Answer a <Collection> of selectors begining with aString"

	^ self interpreterClass selectors
		select: [ : selector | selector beginsWith: aString ]
]
