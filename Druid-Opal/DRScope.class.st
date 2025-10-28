Class {
	#name : 'DRScope',
	#superclass : 'Object',
	#instVars : [
		'argumentNames',
		'numArgs',
		'tempVars',
		'copiedVars',
		'tempVector',
		'id',
		'tempVectorVar',
		'node',
		'outerScope',
		'inlinedScopes'
	],
	#category : 'Druid-Opal-IR',
	#package : 'Druid-Opal',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRScope >> addArguments: anObject [

	argumentNames addAll: anObject.
]

{ #category : 'adding' }
DRScope >> addInlinedScope: aDRScope [ 

	self assert: aDRScope outerScope = self.

	inlinedScopes add: aDRScope 
]

{ #category : 'accessing' }
DRScope >> addTemps: aCollection [

	tempVector addAll: aCollection.
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

	"For now, follow Opal structure, will change this incrementally"

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

{ #category : 'testing' }
DRScope >> isEmpty [

	tempVars isEmpty ifFalse: [ ^ false ].
	tempVector isEmpty ifFalse: [ ^ false ].
	^ true
]

{ #category : 'accessing' }
DRScope >> lookupVar: name [

	^ self lookupVar: name ifNone: [ self unexplored ]
]

{ #category : 'accessing' }
DRScope >> lookupVar: name ifNone: noneBlock [

	((tempVector includes: name) or: [ argumentNames includes: name ])
		ifTrue: [ ^ self ].

	^ outerScope
		  ifNotNil: [ :os |
			  os lookupVar: name ifNone: [
				  noneBlock cull: name.
				  ^ self ] ]
		  ifNil: [
			  noneBlock cull: name.
			  self ]
]

{ #category : 'accessing' }
DRScope >> lookupVarOrAddNewTemp: name [

	"Remove this?"

	^ self lookupVar: name ifNone: [ self addTemps: { name } ]
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
