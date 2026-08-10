Class {
	#name : 'DRScope',
	#superclass : 'Object',
	#instVars : [
		'tempVars',
		'id',
		'node',
		'outerScope',
		'inlinedScopes',
		'arguments',
		'initInstruction'
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

{ #category : 'inspecting' }
DRScope >> addToScopeList: alreadyComputed [

	(alreadyComputed includes: self) ifTrue: [ ^ alreadyComputed ].

	alreadyComputed add: self.
	self inlinedScopes do: [ :e |
		e addToScopeList: alreadyComputed ]
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
DRScope >> hasMissingInlines [

	inlinedScopes ifNotEmpty: [ ^ false ].

	^ self isSend and: [ node hasMissingInlines ]
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

{ #category : 'accessing' }
DRScope >> initInstruction [
	^ initInstruction
]

{ #category : 'as yet unclassified' }
DRScope >> initInstruction: aDRInitializeScope [

	initInstruction := aDRInitializeScope
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

{ #category : 'inspecting' }
DRScope >> inspectionInlines: aBuilder [
	<inspectorPresentationOrder: 10 title: 'Inlining Graph'>
	
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self withAllInlinedScopes asSet collect: [ :aScope | | g label |
		g := RSGroup new.
		(label := (RSLabel new text: aScope selector asString)
			borderColor: Color black;
			yourself).
		g add: (RSBox new
			color: (aScope hasMissingInlines ifTrue: [ Color red muchLighter ] ifFalse: [ Color white ]);
			border: (RSBorder new 
				color: (aScope node isMessageSend ifTrue: [ Color orange ] ifFalse: [ Color purple ]); 
				width: 2);
			extent: label extent + 10;
			yourself).
		g add: label.
		g asShape
			@ RSDraggable;
			model: aScope;
			yourself ] as: Array.
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #inlinedScopes.
	
	"All the characters are displayed using a grid layout"
	RSDominanceTreeLayout new
		horizontalGap: 50;
		shouldValidateCycles;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassalInspectorPresenter new
		canvas: view;
		yourself
]

{ #category : 'inspecting' }
DRScope >> inspectionSource: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Source'>
	
	^ self node inspectionSource: aBuilder
]

{ #category : 'testing' }
DRScope >> isEmpty [

	arguments isEmpty ifFalse: [ ^ false ].
	tempVars isEmpty ifFalse: [ ^ false ].
	^ true
]

{ #category : 'testing' }
DRScope >> isSend [

	^ node isMessageSend
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

{ #category : 'accessing' }
DRScope >> selector [

	node isBlock ifTrue: [ ^ '[ ]' "??" ].

	^ node selector 
]

{ #category : 'query' }
DRScope >> tempVarNames [

	^ tempVars collect: [ :v | v name ] as: Array
]

{ #category : 'query' }
DRScope >> tempVectorName [
	"the name of the tempVector is not a valid name of a temp variable
	 This way we avoid name clashes "
	| parent |
	parent := outerScope ifNotNil: [ outerScope id asString ] ifNil: [ '0' ].
	^parent, 'vector', id asString
]

{ #category : 'query' }
DRScope >> tempVectorVarNames [ 
	
	^ (self argumentNames , self tempVarNames) asArray 
]

{ #category : 'testing' }
DRScope >> wasMethodInlined [

	outerScope ifNil: [ ^ false ].
	node isMethod ifFalse: [ ^ outerScope wasMethodInlined ].
	
	"If we arrive to a method scope and it's not the last one, it was inlined"
	self assert: outerScope node isMessageSend.
	^ true
]

{ #category : 'inspecting' }
DRScope >> withAllInlinedScopes [
	
	| scopes |
	scopes := OrderedCollection new.
	self addToScopeList: scopes.
	^ scopes
]
