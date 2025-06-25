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
		'opalScope'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRScope >> addArguments: anObject [

	argumentNames addAll: anObject.
]

{ #category : 'adding' }
DRScope >> addCopiedVarsFromOuterScope: aDRScope [
	"pass vectorTemps of outer Scope to the actual one"
	
	aDRScope tempVectorVarNames isNotEmpty ifTrue: [ 
		 copiedVars at: aDRScope tempVectorName put: aDRScope tempVectorVarNames ].
		
	aDRScope copiedVars keysAndValuesDo: [ :k :v | copiedVars at: k put: v ]
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

	^ argumentNames 
]

{ #category : 'accessing' }
DRScope >> copiedVarNames [ 
	
	^ copiedVars keys
]

{ #category : 'accessing' }
DRScope >> copiedVars [

	^ copiedVars
]

{ #category : 'accessing' }
DRScope >> copiedVars: anObject [

	copiedVars := anObject
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
	copiedVars := OrderedDictionary new. "vars from outside: vectorTemp + outer read temp | nil if first scope"
	id := 0
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
DRScope >> opalScope [

	^ opalScope
]

{ #category : 'accessing' }
DRScope >> opalScope: anObject [

	opalScope := anObject
]

{ #category : 'accessing' }
DRScope >> outerScope [

	^ outerScope
]

{ #category : 'accessing' }
DRScope >> outerScope: aScope [
	
	outerScope := aScope.
]

{ #category : 'modifying' }
DRScope >> renameTempVar: oldName into: newName [

	tempVector := tempVector
		              copyReplaceAll: { oldName }
		              with: { newName }
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
