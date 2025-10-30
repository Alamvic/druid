Class {
	#name : 'DRMethodControlFlowGraph',
	#superclass : 'DRControlFlowGraph',
	#instVars : [
		'scope'
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

	scope addTemps: { aName }
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

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope [

	^ scope
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope: anObject [

	scope := anObject
]
