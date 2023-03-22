Trait {
	#name : #TDRMethodSelector,
	#instVars : [
		'allowDuplicates',
		'excludeObjectClass',
		'scanSuperclasses',
		'matchMethod',
		'methods'
	],
	#category : #'Druid-DispatchTable'
}

{ #category : #querying }
TDRMethodSelector >> addFromSelectorPattern: aString [
	"Answer a <Collection> of selectors begining with aString"

	aString ifEmpty: [ ^ Array empty ].
	^ self methods addAll: (self scanSuperclasses
		ifTrue: [ self allSelectorsMatching: aString ]
		ifFalse: [ self selectorsMatching: aString ]).
	
]

{ #category : #'accessing - query' }
TDRMethodSelector >> allMethodsMatching: aString [
	"Answer a <Collection> of <CompilerMethod> beginning with aString, looking up the pattern in the receiver's interpreter class methodDictionary, chase the superclass chain and try again until no method is found."

	| lookupClass allSelectors |
	lookupClass := self targetClass.
	allSelectors := self collectorClass new.
	[ self finishScanIn: lookupClass ] whileFalse: [
		allSelectors addAll: (lookupClass methods select: [ : method |  method selector perform: self matchMethod with: aString ]).
		lookupClass := lookupClass superclass ].
	^ allSelectors
]

{ #category : #'accessing - query' }
TDRMethodSelector >> allSelectorsMatching: aString [
	"Answer a <Collection> of selectors begining with aString, looking up the pattern in the receiver's interpreter class methodDictionary, chase the superclass chain and try again until no method is found."

	| lookupClass allSelectors |
	lookupClass := self interpreterClass.
	allSelectors := self collectorClass new.
	[ lookupClass isNil ] whileFalse: [
		allSelectors addAll: (lookupClass selectors select: [ :selector | selector perform: self matchMethod with: aString ]).
		lookupClass := lookupClass superclass ].
	^ allSelectors
]

{ #category : #accessing }
TDRMethodSelector >> allowDuplicates [

	^ allowDuplicates
]

{ #category : #accessing }
TDRMethodSelector >> allowDuplicates: anObject [

	allowDuplicates := anObject
]

{ #category : #accessing }
TDRMethodSelector >> collectorClass [

	^ self allowDuplicates
		ifTrue: [ Bag ]
		ifFalse: [ OrderedCollection ]
]

{ #category : #defaults }
TDRMethodSelector >> defaultMatchMethod [

	^ #beginsWith:
]

{ #category : #private }
TDRMethodSelector >> excludeObjectClass [

	^ excludeObjectClass
		ifNil: [ excludeObjectClass := true ]
]

{ #category : #private }
TDRMethodSelector >> finishScanIn: lookupClass [

	^ lookupClass isNil or: [ self excludeObjectClass and: [ lookupClass = Object ] ]
]

{ #category : #initialization }
TDRMethodSelector >> initialize [

	super initialize.
	scanSuperclasses := true.
	allowDuplicates := true
]

{ #category : #'accessing - query' }
TDRMethodSelector >> matchMethod [
	"Answer a <Symbol> representing the selector used to compare against the primitive selectors"

	^ matchMethod
		ifNil: [ matchMethod := self defaultMatchMethod ]
]

{ #category : #'accessing - query' }
TDRMethodSelector >> matchMethod: anObject [

	matchMethod := anObject
]

{ #category : #querying }
TDRMethodSelector >> methods [

	^ methods
		ifNil: [ methods := self collectorClass new ]
]

{ #category : #querying }
TDRMethodSelector >> methodsFromPattern: aString [
	"Answer a <Collection> of selectors begining with aString"

	aString ifEmpty: [ ^ Array empty ].
	^ self methods addAll: (self scanSuperclasses
		ifTrue: [ self allMethodsMatching: aString ]
		ifFalse: [ self methodsMatching: aString ])
]

{ #category : #'accessing - query' }
TDRMethodSelector >> methodsMatching: aString [
	"Answer a <Collection> of selectors begining with aString"

	^ self interpreterClass methods
		select: [ : method | method selector perform: self matchMethod with: aString ]
]

{ #category : #accessing }
TDRMethodSelector >> scanSuperclasses [

	^ scanSuperclasses
]

{ #category : #accessing }
TDRMethodSelector >> scanSuperclasses: anObject [

	scanSuperclasses := anObject
]

{ #category : #'accessing - query' }
TDRMethodSelector >> selectorsMatching: aString [
	"Answer a <Collection> of selectors matching aString according to the receiver's match method"

	^ self interpreterClass selectors
		select: [ : selector | selector perform: self matchMethod with: aString ]
]

{ #category : #'accessing - query' }
TDRMethodSelector >> useBeginsWith [
	"Set the receiver to use regular expressions to match queries"

	self matchMethod: #beginsWith:
]

{ #category : #'accessing - query' }
TDRMethodSelector >> useRegEx [
	"Set the receiver to use regular expressions to match queries"

	self matchMethod: #matchesRegex:
]
