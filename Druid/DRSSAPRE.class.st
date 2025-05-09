Class {
	#name : 'DRSSAPRE',
	#superclass : 'DRControlFlowOptimisation',
	#instVars : [
		'dominatorTree'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRSSAPRE >> doApply: cfg [
	
	self lookForRedundancy: cfg
]

{ #category : 'initialization' }
DRSSAPRE >> initialize [

	super initialize.
	dominatorTree := DRDominatorTree new.
]

{ #category : 'as yet unclassified' }
DRSSAPRE >> insertPhi: cfg [
	" when an expression appears, inserting a PHi at its iterated dominance frointer "

	cfg blocksInReversePostOrderDo: [ :b | 
		b instructionsDo: [ :i | 
			i isCopy ifTrue: [ 
				(cfg dominatorTree iteratedDominanceFrontierOf: b) asSet do: [ :bl | 
					bl addInstructionFirst: bl newPhi ] ] ] ]
]

{ #category : 'as yet unclassified' }
DRSSAPRE >> lookForRedundancy: cfg [

	| visitedInstructions |
	visitedInstructions := OrderedCollection new.
	cfg blocksInReversePostOrderDo: [ :b | 
		b instructionsDo: [ :i | | replacement |
			replacement := visitedInstructions
				detect: [ :each | (i isCopy and: [ i hasSameExpressionAs: each ]) ]
				ifNone: [ nil ]. 

			replacement ifNotNil: [ i replaceOperandAtIndex: 1 by: replacement ].
			visitedInstructions add: i ] ]
]

{ #category : 'as yet unclassified' }
DRSSAPRE >> renamePhi: cfg [

		cfg blocksInReversePostOrderDo: [ :b | 
		b instructionsDo: [ :i | 
			i isPhiFunction ifTrue: [
				i nextInstruction replaceOperandAtIndex: 1 by: i ] ] ]
	


]
