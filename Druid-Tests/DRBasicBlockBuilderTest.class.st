Class {
	#name : #DRBasicBlockBuilderTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRBasicBlockBuilderTest >> testBlockHasSequentialId [

	| cfg |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlock.
	self assert: cfg newBasicBlock id equals: 2
]

{ #category : #tests }
DRBasicBlockBuilderTest >> testBlockInitialSequenceId [

	| cfg |
	cfg := DRControlFlowGraph new.
	self assert: cfg newBasicBlock id equals: 1
]

{ #category : #tests }
DRBasicBlockBuilderTest >> testRemovingBlockKeepsSequence [

	| cfg blockToRemove |
	cfg := DRControlFlowGraph new.
	blockToRemove := cfg newBasicBlock.
	cfg removeBasicBlock: blockToRemove.
	
	self assert: cfg newBasicBlock id equals: 2
]
