Class {
	#name : 'DRMethodControlFlowGraph',
	#superclass : 'DRControlFlowGraph',
	#instVars : [
		'scope',
		'pragmas'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'accessing' }
DRMethodControlFlowGraph >> addScopeToInstructionIfNeeded: anInstruction [

	anInstruction scope: self scope.
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> addTempNameToScope: aName [

	scope addTempNames: { aName }
]

{ #category : 'queries' }
DRMethodControlFlowGraph >> allBlockClosures [

	| closures |
	closures := OrderedCollection new.
	self instructionsDo: [ :i |
		i operands do: [ :op |
			op isDRBlockClosure ifTrue: [ closures add: op ] ] ].
	^ closures
]

{ #category : 'queries' }
DRMethodControlFlowGraph >> allInstructionsWithScope: aDRScope [

	| cfgInstructions blockInstructions |
	cfgInstructions := self instructions select: [ :i |
		                   i isScopedInstruction and: [ i scope = aDRScope ] ].

	blockInstructions := self allBlockClosures flatCollect: [ :b |
		                     b ensureIR allInstructionsWithScope: aDRScope ].

	^ cfgInstructions , blockInstructions
]

{ #category : 'queries' }
DRMethodControlFlowGraph >> initialize [ 

	super initialize .
	pragmas := OrderedCollection new.
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> pragmas [

	^ pragmas
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> pragmas: anObject [

	pragmas := anObject
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> primitiveFromPragma [ 

	^ pragmas
		detect: [ :each | each isPrimitive ]
		ifFound: [ :aPragmaPrimitive | aPragmaPrimitive asIRPrimitive ]
		ifNone: [ OCIRPrimitive null ]
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope [

	^ scope
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope: anObject [

	scope := anObject
]
