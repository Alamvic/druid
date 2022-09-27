Class {
	#name : #DRHyperBlock,
	#superclass : #Object,
	#instVars : [
		'blocks'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRHyperBlock class >> blocks: aCollection [
	
	^ self new
		blocks: aCollection;
		yourself
]

{ #category : #'control-flow' }
DRHyperBlock >> addBetween: sourceBlock and: targetBlock [

	| newFirstBlock newLastBlock currentTarget |
	newFirstBlock := self entryBlock.
	newLastBlock := self exitBlock.
	sourceBlock replaceSuccessor: targetBlock by: newFirstBlock.
	targetBlock replacePredecessor: sourceBlock by: newLastBlock.
	newFirstBlock addPredecessor: sourceBlock.
	currentTarget := newLastBlock endInstruction targets detect: [ :b | (blocks includes: b) not ].
	newLastBlock endInstruction replaceTarget: currentTarget by: targetBlock.
	self controlFlowGraph fixBackJumps
]

{ #category : #accessing }
DRHyperBlock >> blocks: aCollection [ 
	blocks := aCollection
]

{ #category : #accessing }
DRHyperBlock >> controlFlowGraph [

	^ self entryBlock controlFlowGraph
]

{ #category : #transformations }
DRHyperBlock >> duplicate [
	
	self hasSingleExit ifFalse: [ self error: 'Cannot duplicate if more than one exit' ].
	
	^ DRBlockDuplicator new
		controlFlowGraph: blocks anyOne controlFlowGraph;
		duplicate: blocks
]

{ #category : #accessing }
DRHyperBlock >> endInstruction [

	^ self exitBlock endInstruction
]

{ #category : #accessing }
DRHyperBlock >> entryBlock [

	^ self hasSingleEntry
		  ifTrue: [ ^ self entryBlocks first ]
		  ifFalse: [ blocks first ]
]

{ #category : #testing }
DRHyperBlock >> entryBlocks [

	^ blocks select: [ :block | 
		  block predecessors isEmpty or: [ 
			  block predecessors anySatisfy: [ :p | (blocks includes: p) not ] ] ]
]

{ #category : #accessing }
DRHyperBlock >> exitBlock [

	^ self hasSingleExit
		  ifTrue: [ ^ self exitBlocks first ]
		  ifFalse: [ blocks last ]
]

{ #category : #testing }
DRHyperBlock >> exitBlocks [

	^ blocks select: [ :e | 
		  e successors anySatisfy: [ :s | (blocks includes: s) not ] ]
]

{ #category : #testing }
DRHyperBlock >> hasSingleEntry [

	^ self entryBlocks size = 1
]

{ #category : #testing }
DRHyperBlock >> hasSingleExit [

	^ self exitBlocks size = 1
]

{ #category : #accessing }
DRHyperBlock >> instructions [
	^ blocks flatCollect: [ :block | block instructions ]
]

{ #category : #accessing }
DRHyperBlock >> predecessors [
	
	^ self entryBlock predecessors
]

{ #category : #accessing }
DRHyperBlock >> removePredecessor: aDRBasicBlock [ 
	
	| index |
	index := self entryBlock predecessors indexOf: aDRBasicBlock.
	self entryBlock removePredecessorAt: index
]

{ #category : #transformation }
DRHyperBlock >> tailDuplicate [

	"We can only tail duplicate if the instructions do not escape"
	self hasSingleExit ifFalse: [ 
		self error: 'cannot tail duplicate if we have more than one  exit' ].
	
	self predecessors allButFirst do: [ :predecessorToRewire |
		| duplicatedBlocks duplicatedEntryBlock duplicatedExitBlock |
		duplicatedBlocks := self duplicate.
		duplicatedEntryBlock := duplicatedBlocks at: self entryBlock.

		self predecessors do: [ :predecessorToRemove |
			predecessorToRemove = predecessorToRewire ifFalse: [ 
				duplicatedEntryBlock removePredecessor: predecessorToRemove ] ].
		
		self entryBlock removePredecessor: predecessorToRewire.
		predecessorToRewire replaceSuccessor: self entryBlock by: duplicatedEntryBlock.
		duplicatedExitBlock := duplicatedBlocks at: self exitBlock.
		
		duplicatedExitBlock successors do: [ :successor |
			successor addPredecessor: duplicatedExitBlock.
			duplicatedBlocks valuesDo: [ :b |
				b instructionsDo: [ :i |
					i users do: [ :user |
						(user isPhiFunction and: [  user basicBlock = successor ])
							ifTrue: [ user addValue: i forPredecessor: duplicatedExitBlock ].
					]
				]
			]
		].
	].
]
