Class {
	#name : #DRSSAPRE,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRSSAPRE >> doApply: cfg [


	self lookForRedundancy: cfg
]

{ #category : #initialization }
DRSSAPRE >> initialize [

	super initialize.
	
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
