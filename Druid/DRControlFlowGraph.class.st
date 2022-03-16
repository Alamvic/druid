Class {
	#name : #DRControlFlowGraph,
	#superclass : #Object,
	#instVars : [
		'initialBasicBlock',
		'basicBlocks',
		'nextBasicBlockId',
		'instructionFactory',
		'edges'
	],
	#category : #'Druid-IR'
}

{ #category : #iterating }
DRControlFlowGraph >> addBasicBlock: aBlock [

	self assert: (basicBlocks noneSatisfy: [:b | b id = aBlock id]).
	basicBlocks add: aBlock
]

{ #category : #edges }
DRControlFlowGraph >> addEdge: anEdge [

	edges add: anEdge
]

{ #category : #edges }
DRControlFlowGraph >> addEdgeFrom: sourceBlock to: destinationBlock [
	
	| edge |
	edge := DRRegistersInfo withSource: sourceBlock andDestination: destinationBlock.
	self addEdge: edge.
	^ edge
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

{ #category : #accessing }
DRControlFlowGraph >> blockById: anInteger [ 
	
	^ self blocks detect: [ :e | e id = anInteger ]
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

{ #category : #'as yet unclassified' }
DRControlFlowGraph >> clearPaths [

	edges := OrderedCollection new
]

{ #category : #'as yet unclassified' }
DRControlFlowGraph >> deadPaths [

	| paths |
	paths := OrderedDictionary new.
	edges select: [ :e | e isDead ] thenDo: [ :e | 
		paths
			at: e deadTarget
			ifPresent: [ :collection | collection add: e ]
			ifAbsentPut: { e } asOrderedCollection ].
	^ paths
]

{ #category : #accessing }
DRControlFlowGraph >> edges [

	^ edges
]

{ #category : #edges }
DRControlFlowGraph >> edgesFrom: sourceBlock to: destinationBlock [

	^ edges select: [ :e | e source = sourceBlock and: [e destination = destinationBlock] ]
]

{ #category : #accessing }
DRControlFlowGraph >> firstBasicBlock [

	^ initialBasicBlock successor
]

{ #category : #edges }
DRControlFlowGraph >> incomingEdgeFor: aBlock [

	^ (self incomingEdgesFor: aBlock) first "Check?"
]

{ #category : #edges }
DRControlFlowGraph >> incomingEdgesFor: aBlock [

	^ edges select: [ :e | e destination = aBlock ]
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
	edges := OrderedCollection new.
	initialBasicBlock := DRBasicBlock id: 0.
	nextBasicBlockId := 1.
]

{ #category : #inspecting }
DRControlFlowGraph >> inspectionCFG [
	<inspectorPresentationOrder: 0 title: 'CFG'> 
	| view blockViews eb controller |
	view := RSCanvas new.
	blockViews := self blocks collect: [ :b | 
		| label box |
		box := RSBox new
			noPaint;
			border: (RSBorder new 
				width: 2; 
				color: Color black;
				yourself);
			cornerRadius: 2;
			yourself.
		label := RSLabel new 
			text: b id asString;
			color: Color black;
			yourself.
		box extent: 30@30.
		{ box. label } asShape
			@ RSDraggable;
			@ (RSPopup text: [:model | 
				String streamContents: [:s | (model instructions collect: #prettyPrint)
					asStringOn: s delimiter: String cr] 
				]);
			@ (RSMenuActivable new menuDo: [ :menu :element | 
				menu
					add: 'Tail-duplicate'
					target: element model
					selector: #tailDuplicate ]);
			model: b;
			yourself ].
	view addAll: blockViews.
	
	eb := RSLineBuilder arrowedLine.
	eb canvas: view.
	eb color: Color blue muchDarker.
	eb width: 2.
	eb withBorderAttachPoint.
	eb 	shapes: blockViews.
	eb connectToAll: #successors.
	view lines do: [ :line | line model.
		line @ (RSPopup text: [ :model | 
			String streamContents: [:s | 
				(self edgesFrom: model key to: model value) 
					do: [ :e | e constraints values asStringOn: s delimiter: String cr]
					separatedBy: [s << (String cr , '--o--o--o--o--o--o--o--' , String cr)]
 				]
			])
		].
	
	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.
	controller := RSCanvasController new.
	controller noLegend.
	view newAnimation
		duration: 500 milliSeconds;
		onStepDo: [ :t | view signalUpdate ];
		when: RSAnimationEndEvent do: [:evt | 
			controller configuration noZoomToFitOnExtendChanged ].
	
	view @ controller.
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #accessing }
DRControlFlowGraph >> instructionByResultId: anInteger [ 

	^ self instructions detect: [ :i | i result ifNil: [ false ] ifNotNil: [ i result id = anInteger] ]
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

{ #category : #accessing }
DRControlFlowGraph >> lastBasicBlock [

	^ self reversePostOrderBlocks last
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

{ #category : #edges }
DRControlFlowGraph >> outgoingEdgeFor: aBlock [

	^ (self outgoingEdgesFor: aBlock) first
]

{ #category : #edges }
DRControlFlowGraph >> outgoingEdgesFor: aBlock [

	^ edges select: [ :e | e source = aBlock ]
]

{ #category : #accessing }
DRControlFlowGraph >> phiFunctions [

	^ self instructions select: [ :i | i isPhiFunction ]
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
