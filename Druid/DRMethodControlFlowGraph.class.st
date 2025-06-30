Class {
	#name : 'DRMethodControlFlowGraph',
	#superclass : 'DRControlFlowGraph',
	#instVars : [
		'scope',
		'executionStack'
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
DRMethodControlFlowGraph >> allInstructionsWithScope: aDRScope [

	^ self instructions select: [ :i |
		  i isScopedInstruction and: [ i scope = aDRScope ] ]
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> executionStack [

	^ executionStack
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> executionStack: anObject [

	executionStack := anObject
]

{ #category : 'scopes' }
DRMethodControlFlowGraph >> mergeScope: innerScope into: outerScope [

	| instructions |
	outerScope addTemps: innerScope tempVarNames.

	instructions := self allInstructionsWithScope: innerScope.
	instructions do: [ :i | i scope: outerScope ]
]

{ #category : 'scopes' }
DRMethodControlFlowGraph >> renameTempVar: originalName from: aDRScope into: newName [

	| instructions |
	instructions := self allInstructionsWithScope: aDRScope.
	instructions do: [ :i |
		i address value = originalName ifTrue: [
			i address: newName asDRValue ] ].

	aDRScope renameTempVar: originalName into: newName
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope [

	^ scope
]

{ #category : 'accessing' }
DRMethodControlFlowGraph >> scope: anObject [

	scope := anObject
]
