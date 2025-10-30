Class {
	#name : 'DRScope',
	#superclass : 'Object',
	#instVars : [
		'argumentNames',
		'numArgs',
		'tempVars',
		'tempVector',
		'id',
		'node',
		'outerScope',
		'inlinedScopes',
		'arguments'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'adding' }
DRScope >> addArgumentVariables: variables [

	arguments addAll: variables.
	argumentNames addAll: (variables collect: [ :v | v name ])
]

{ #category : 'adding' }
DRScope >> addInlinedScope: aDRScope [ 

	self assert: aDRScope outerScope = self.

	inlinedScopes add: aDRScope 
]

{ #category : 'adding' }
DRScope >> addTempVariabless: variables [ 

	tempVars addAll: variables.
	tempVector addAll: (variables collect: [:v | v name])
]

{ #category : 'accessing' }
DRScope >> addTemps: aCollection [

	tempVector addAll: aCollection.
]

{ #category : 'as yet unclassified' }
DRScope >> allTempVarNames [

	^ inlinedScopes
		  inject: self tempVarNames
		  into: [ :temps :scope | temps , scope allTempVarNames ]
]

{ #category : 'as yet unclassified' }
DRScope >> allTempsWithOuterTemps [

	^ self tempVectorVarNames , self copiedVars values flattened
]

{ #category : 'accessing' }
DRScope >> argumentNames [

	^ argumentNames asArray 
]

{ #category : 'accessing' }
DRScope >> copiedVarNames [ 
	
	^ self copiedVars keys
]

{ #category : 'accessing' }
DRScope >> copiedVars [

	self outerScope ifNil: [ ^ OrderedDictionary new ].

	^ self outerScope copiedVars, self outerScope definedTempVectors
]

{ #category : 'as yet unclassified' }
DRScope >> defineTempVector [

	| tempVec |
	tempVec := self tempVectorVarNames
		           ifEmpty: [ {  } ]
		           ifNotEmpty: [ :temps |
		           { (self tempVectorName -> temps) } ].

	^ tempVec asOrderedDictionary
]

{ #category : 'query' }
DRScope >> definedTempVectors [

	^ self defineTempVector", self inlinedTempVectors"
]

{ #category : 'accessing' }
DRScope >> hasTempVector [

	^ true
]

{ #category : 'accessing' }
DRScope >> id [

	^ id
]

{ #category : 'accessing' }
DRScope >> id: anObject [

	id := anObject
]

{ #category : 'initialization' }
DRScope >> initialize [

	arguments := OrderedCollection new.
	argumentNames := OrderedCollection new.
	tempVars :=  OrderedCollection new. "local temporaries"
	tempVector  := OrderedCollection new. "vars in the tempVector of this scope"

	inlinedScopes  := OrderedCollection new.
	
	id := 0
]

{ #category : 'accessing' }
DRScope >> inlinedScopes [

	^ inlinedScopes
]

{ #category : 'as yet unclassified' }
DRScope >> inlinedTempVectors [

	^ inlinedScopes
		  inject: OrderedDictionary new
		  into: [ :vectors :scope | vectors , scope definedTempVectors ]
]

{ #category : 'as yet unclassified' }
DRScope >> innerScopes [
	self shouldBeImplemented.
]

{ #category : 'testing' }
DRScope >> isEmpty [

	arguments isEmpty ifFalse: [ ^ false ].
	tempVars isEmpty ifFalse: [ ^ false ].
	tempVector isEmpty ifFalse: [ ^ false ].
	^ true
]

{ #category : 'as yet unclassified' }
DRScope >> lookupScopeDefining: aVariable [

	^ self lookupScopeDefining: aVariable ifNone: [ self unexplored ]
]

{ #category : 'as yet unclassified' }
DRScope >> lookupScopeDefining: aVariable ifNone: noneBlock [

	((tempVars includes: aVariable) or: [ arguments includes: aVariable ])
		ifTrue: [ ^ self ].

	^ outerScope
		  ifNotNil: [ :os |
			  os lookupScopeDefining: aVariable ifNone: [
				  noneBlock cull: aVariable.
				  ^ self ] ]
		  ifNil: [
			  noneBlock cull: aVariable.
			  self ]
]

{ #category : 'accessing' }
DRScope >> node [

	^ node
]

{ #category : 'accessing' }
DRScope >> node: anObject [

	node := anObject
]

{ #category : 'accessing' }
DRScope >> numArgs [

	^ numArgs
]

{ #category : 'accessing' }
DRScope >> numArgs: anObject [

	numArgs := anObject
]

{ #category : 'accessing' }
DRScope >> outerScope [

	^ outerScope
]

{ #category : 'accessing' }
DRScope >> outerScope: aScope [
	
	outerScope := aScope.
]

{ #category : 'accessing' }
DRScope >> tempVarNames [ 
	
	^ tempVector asArray 
]

{ #category : 'accessing' }
DRScope >> tempVectorName [
	"the name of the tempVector is not a valid name of a temp variable
	 This way we avoid name clashes "
	^'0vector', id asString
]

{ #category : 'accessing' }
DRScope >> tempVectorVarNames [ 
	
	^ (argumentNames , tempVector) asArray 
]
