Class {
	#name : #DRControlFlowGraph,
	#superclass : #Object,
	#instVars : [
		'initialBasicBlock',
		'basicBlocks',
		'nextBasicBlockId',
		'instructionFactory'
	],
	#category : #'Druid-IR'
}

{ #category : #iterating }
DRControlFlowGraph >> addBasicBlock: aBlock [

	self assert: (basicBlocks noneSatisfy: [:b | b id = aBlock id]).
	basicBlocks add: aBlock
]

{ #category : #building }
DRControlFlowGraph >> allocateBasicBlockID [
	
	| allocatedID |
	allocatedID := nextBasicBlockId.
	nextBasicBlockId := nextBasicBlockId + 1.
	^ allocatedID
]

{ #category : #building }
DRControlFlowGraph >> allocateTemporaryRegister [
	
	^ instructionFactory allocateTemporaryRegister
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

{ #category : #iterating }
DRControlFlowGraph >> initialize [

	super initialize.
	instructionFactory := DRInstructionFactory new.
	
	basicBlocks := OrderedCollection new.
	initialBasicBlock := DRBasicBlock id: 0.
	nextBasicBlockId := 1.
]

{ #category : #'as yet unclassified' }
DRControlFlowGraph >> inspectionCFG: aBuilder [
	<inspectorPresentationOrder: 0 title: 'CFG'> 
	| view blockViews eb |
	view := RSCanvas new.
	blockViews := self blocks collect: [ :c | | g |
		g := RSGroup new.
		g add: (RSBox new
			size: 50;
			color: Color veryVeryLightGray;
			yourself).
		g add: (RSLabel new text: c id asString).
		g asShape
			@ RSDraggable;
			model: c;
			yourself ].
	view addAll: blockViews.
	
	eb := RSEdgeBuilder arrowedLine.
	eb canvas: view.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #successors.
	
	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.
	view @ RSCanvasController.
	
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #accessing }
DRControlFlowGraph >> instructionFactory [

	^ instructionFactory
]

{ #category : #accessing }
DRControlFlowGraph >> instructionFactory: anObject [

	instructionFactory := anObject
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
	newBlock := DRBasicBlock id: self allocateBasicBlockID.
	newBlock controlFlowGraph: self.
	self addBasicBlock: newBlock.
	^ newBlock
]

{ #category : #building }
DRControlFlowGraph >> newBasicBlockWith: aFullBlockClosure [ 
	
	| newBlock |
	newBlock := self newBasicBlock.
	aFullBlockClosure value: newBlock.
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
