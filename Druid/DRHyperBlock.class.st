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

{ #category : #accessing }
DRHyperBlock >> blocks: aCollection [ 
	blocks := aCollection
]

{ #category : #transformations }
DRHyperBlock >> duplicate [
	
	self hasSingleExit ifFalse: [ self error: 'Cannot duplicate if more than one exit' ].
	
	^ DRBlockDuplicator new
		controlFlowGraph: blocks anyOne controlFlowGraph;
		duplicate: blocks
]

{ #category : #accessing }
DRHyperBlock >> entryBlock [
	
	^ blocks first
]

{ #category : #accessing }
DRHyperBlock >> exitBlock [
	
	^ blocks last
]

{ #category : #testing }
DRHyperBlock >> hasSingleExit [
	
	| exitBlocks |
	exitBlocks := blocks count: [ :e | 
		e successors anySatisfy: [ :s | 
			(blocks includes: s) not ] ].
	^ exitBlocks = 1
]

{ #category : #accessing }
DRHyperBlock >> predecessors [
	
	^ self entryBlock predecessors
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
