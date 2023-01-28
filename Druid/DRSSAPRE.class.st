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

	| visitedInstructions stream hasBeenReplaced |
	visitedInstructions := OrderedCollection new.
	cfg blocksInReversePostOrderDo: [ :b | 
		b instructionsDo: [ :i | 
			stream := visitedInstructions readStream.
			hasBeenReplaced := false.
			[ hasBeenReplaced not and: [ stream atEnd not ] ] whileTrue: [ 
				| vi |
				vi := stream next.
				(i isCopy and: [ i hasSameExpressionAs: vi ]) ifTrue: [ 
					i replaceOperandAtIndex: 1 by: vi.
					hasBeenReplaced := true ] ].
			visitedInstructions add: i ] ]
]
