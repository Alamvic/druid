Class {
	#name : 'DRScope',
	#superclass : 'Object',
	#instVars : [
		'tempVars',
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

{ #category : 'configure' }
DRScope >> addArgumentVariables: variables [

	arguments addAll: variables
]

{ #category : 'configure' }
DRScope >> addInlinedScope: aDRScope [ 

	self assert: aDRScope outerScope = self.

	inlinedScopes add: aDRScope 
]

{ #category : 'configure' }
DRScope >> addTempNames: names [

	tempVars addAll:
		(names collect: [ :name | TemporaryVariable named: name ])
]

{ #category : 'configure' }
DRScope >> addTempVariables: variables [ 

	tempVars addAll: variables
]

{ #category : 'query' }
DRScope >> allTempVarNames [

	^ inlinedScopes
		  inject: self tempVarNames
		  into: [ :temps :scope | temps , scope allTempVarNames ]
]

{ #category : 'query' }
DRScope >> allTempsWithOuterTemps [

	^ self tempVectorVarNames , self copiedVars values flattened
]

{ #category : 'query' }
DRScope >> argumentNames [

	^ arguments collect: [ :v | v name ] as: Array
]

{ #category : 'query' }
DRScope >> copiedVarNames [ 
	
	^ self copiedVars keys
]

{ #category : 'query' }
DRScope >> copiedVars [

	self outerScope ifNil: [ ^ OrderedDictionary new ].

	^ self outerScope copiedVars, self outerScope definedTempVectors
]

{ #category : 'query' }
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

{ #category : 'testing' }
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
	tempVars := OrderedCollection new.
	inlinedScopes := OrderedCollection new.

	id := 0
]

{ #category : 'accessing' }
DRScope >> inlinedScopes [

	^ inlinedScopes
]

{ #category : 'query' }
DRScope >> inlinedTempVectors [

	^ inlinedScopes
		  inject: OrderedDictionary new
		  into: [ :vectors :scope | vectors , scope definedTempVectors ]
]

{ #category : 'testing' }
DRScope >> isEmpty [

	arguments isEmpty ifFalse: [ ^ false ].
	tempVars isEmpty ifFalse: [ ^ false ].
	^ true
]

{ #category : 'query' }
DRScope >> lookupScopeDefining: aVariable [

	^ self lookupScopeDefining: aVariable ifNone: [ self unexplored ]
]

{ #category : 'query' }
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
DRScope >> outerScope [

	^ outerScope
]

{ #category : 'accessing' }
DRScope >> outerScope: aScope [
	
	outerScope := aScope.
]

{ #category : 'query' }
DRScope >> tempVarNames [

	^ tempVars collect: [ :v | v name ] as: Array
]

{ #category : 'query' }
DRScope >> tempVectorName [
	"the name of the tempVector is not a valid name of a temp variable
	 This way we avoid name clashes "
	^'0vector', id asString
]

{ #category : 'query' }
DRScope >> tempVectorVarNames [ 
	
	^ (self argumentNames , self tempVarNames) asArray 
]
