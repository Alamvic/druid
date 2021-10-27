Class {
	#name : #DROptimisationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #testing }
DROptimisationTest class >> isAbstract [

	^ self class == DROptimisationTest
]

{ #category : #test }
DROptimisationTest >> setUpCFG: n [

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg initialBasicBlock.
	cfg b0: b0.
	
	b1 := cfg newBasicBlock.
	cfg b1: b1.
	b0 jumpTo: b1.
	n = 1 ifTrue: [ ^ cfg ].
	
	b2 := cfg newBasicBlock.
	cfg b2: b2.
	b1 jumpTo: b2.
	n = 2 ifTrue: [ ^ cfg ].

	b3 := cfg newBasicBlock.
	cfg b3: b3.
	b2 jumpTo: b3.
	n = 3 ifTrue: [ ^ cfg ].

	self error: 'Unsupported graph size: ', n asString
]
