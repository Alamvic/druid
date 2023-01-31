Class {
	#name : #DRSSAPRE,
	#superclass : #DRControlFlowOptimisation,
	#instVars : [
		'dominatorTree'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRSSAPRE >> doApply: cfg [
	
	self lookForRedundancy: cfg
]

{ #category : #initialization }
DRSSAPRE >> initialize [

	super initialize.
	dominatorTree := DRDominatorTree new.
]

{ #category : #'as yet unclassified' }
DRSSAPRE >> insertPhi: cfg [

	| tempList |
	tempList:= OrderedCollection new.
	 
	cfg blocksInReversePostOrderDo: [ :b | 
		b predecessors size > 1 ifTrue: [ 
			b instructions do: [ :i | 
				i isCopy ifTrue: [ 
					( tempList add: (b instructionFactory 
						 phiWith: (b predecessorAt: 1) firstInstruction
						 with: (b predecessorAt: 2) firstInstruction))
				]
			].
		   tempList do: [ :p | b addInstruction: p ] 
		] 
	]
]

{ #category : #'as yet unclassified' }
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

{ #category : #'as yet unclassified' }
DRSSAPRE >> renamePhi: cfg [

		cfg blocksInReversePostOrderDo: [ :b | 
		b instructionsDo: [ :i | 
			i isPhiFunction ifTrue: [
				i nextInstruction replaceOperandAtIndex: 1 by: i ] ] ]
]
