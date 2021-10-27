Class {
	#name : #DRControlFlowGraph,
	#superclass : #Object,
	#instVars : [
		'initialBasicBlock',
		'basicBlocks'
	],
	#category : #'Druid-IR'
}

{ #category : #iterating }
DRControlFlowGraph >> addBasicBlock: aBlock [

	basicBlocks add: aBlock
]

{ #category : #iterating }
DRControlFlowGraph >> blocks [

	^ basicBlocks
]

{ #category : #iterating }
DRControlFlowGraph >> blocksDo: aFullBlockClosure [ 

	basicBlocks do: aFullBlockClosure
]

{ #category : #iterating }
DRControlFlowGraph >> blocksInReversePostOrderDo: aFullBlockClosure [ 

	| workingQueue visited visitsToThisBlockSoFar |
	visited := Dictionary new.
	
	
	workingQueue := OrderedCollection new.
	workingQueue add: self initialBasicBlock.
	
	[ workingQueue isEmpty ] whileFalse: [ | currentBlock |
		currentBlock := workingQueue removeFirst.
		aFullBlockClosure value: currentBlock.
		currentBlock hasFinalInstruction ifTrue: [
			currentBlock lastInstruction targets do: [ :target |
				
				"Count how many times we have visited this block"
				visitsToThisBlockSoFar := visited
					at: target
					ifPresent: [ :p | p + 1 ]
					ifAbsent: [ 1 ].
				visited at: target put: visitsToThisBlockSoFar.
				
				visitsToThisBlockSoFar = target numberOfPredecessors ifTrue: [ 
					workingQueue addLast: target  ] ] ] ]
]

{ #category : #accessing }
DRControlFlowGraph >> initialBasicBlock [ 
	^ initialBasicBlock
]

{ #category : #accessing }
DRControlFlowGraph >> initialBasicBlock: aDRBasicBlock [ 
	initialBasicBlock := aDRBasicBlock
]

{ #category : #iterating }
DRControlFlowGraph >> initialize [

	super initialize.
	basicBlocks := OrderedCollection new
]

{ #category : #accessing }
DRControlFlowGraph >> instructions [
	
	| instructions |
	instructions := OrderedCollection new.
	self instructionsDo: [ :e |
		instructions add: e ].
	^ instructions
]

{ #category : #iterating }
DRControlFlowGraph >> instructionsDo: aFullBlockClosure [

	self blocksDo: [ :b | 
		b instructionsDo: [ :e | aFullBlockClosure value: e ] ]
]

{ #category : #building }
DRControlFlowGraph >> newBasicBlock [
	
	| newBlock |
	newBlock := DRBasicBlock id: basicBlocks size.
	self addBasicBlock: newBlock.
	^ newBlock
]

{ #category : #iterating }
DRControlFlowGraph >> postOrderBlocks [

	"Return the list of blocks based on a simple linearization algorithm"

	^ self reversePostOrderBlocks reversed
]

{ #category : #removing }
DRControlFlowGraph >> removeBasicBlock: aDRBasicBlock [ 
	
	basicBlocks remove: aDRBasicBlock
]

{ #category : #iterating }
DRControlFlowGraph >> removeInstruction: aDRInstruction [
	
	self blocksInReversePostOrderDo: [ :b | b instructions do: [ :i | i = aDRInstruction ifTrue: [ b removeInstruction: aDRInstruction ] ] ]
]

{ #category : #iterating }
DRControlFlowGraph >> reversePostOrderBlocks [

	"Return the list of blocks based on a simple linearization algorithm"

	| blocks |
	blocks := OrderedCollection new.
	self blocksInReversePostOrderDo: [ :b | blocks add: b ].
	^ blocks
]

{ #category : #validating }
DRControlFlowGraph >> validate [
	
	self blocks do: [ :e | e validate ]
]

{ #category : #validating }
DRControlFlowGraph >> validateBlock: aDRBasicBlock [ 
	
	aDRBasicBlock predecessors do: [ :e |
		(e successors includes: aDRBasicBlock)
			ifFalse: [ self error: 'Predecessor-successor mismatch' ] ].
		
	aDRBasicBlock successors do: [ :e |
		(e predecessors includes: aDRBasicBlock)
			ifFalse: [ self error: 'Predecessor-successor mismatch' ] ].
]
