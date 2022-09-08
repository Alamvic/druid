Class {
	#name : #DRControlFlowGraph,
	#superclass : #Object,
	#instVars : [
		'initialBasicBlock',
		'initialConstraint',
		'basicBlocks',
		'nextBasicBlockId',
		'instructionFactory',
		'edges',
		'deadPaths',
		'numberOfSpillSlots',
		'tree',
		'edgesBySource'
	],
	#category : #'Druid-IR'
}

{ #category : #iterating }
DRControlFlowGraph >> addBasicBlock: aBlock [

	self assert: (basicBlocks noneSatisfy: [:b | b id = aBlock id]).
	basicBlocks add: aBlock.
	^ aBlock
]

{ #category : #edges }
DRControlFlowGraph >> addEdge: anEdge [

	edges add: anEdge.
	(edgesBySource
		 at: anEdge source
		 ifAbsentPut: [ OrderedCollection new ]) add: anEdge
]

{ #category : #edges }
DRControlFlowGraph >> addEdgeFrom: sourceBlock to: destinationBlock branchIndex: branchIndex [

	| edge |
	edge := DRCPSEdge withSource: sourceBlock andDestination: destinationBlock branchIndex: branchIndex.
	self addEdge: edge.
	^ edge
]

{ #category : #accessing }
DRControlFlowGraph >> allBackJumps [

	^ self reversePostOrderBlocks
		  collect: [ :b | b endInstruction ]
		  thenSelect: [ :i | i isBackJump ]
]

{ #category : #accessing }
DRControlFlowGraph >> allConditionalJumps [

	^ self reversePostOrderBlocks
		  collect: [ :b | b endInstruction ]
		  thenSelect: [ :i | i isConditionalBranch ]
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

{ #category : #optimization }
DRControlFlowGraph >> applyOptimisation: optimisation [

	optimisation applyTo: self.
	self validate
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
				
				visitsToThisBlockSoFar = target numberOfForwardPredecessors ifTrue: [ 
					workingQueue addFirst: target  ] ] ] ]
]

{ #category : #dominance }
DRControlFlowGraph >> buildDominatorTree [

	self tree: (DRDominatorTree new buildDominatorTreeOf: self).
	self tree computeDominanceFrontier
]

{ #category : #edges }
DRControlFlowGraph >> clearEdges [

	edges := OrderedCollection new.
	edgesBySource := Dictionary new.
	deadPaths := nil
]

{ #category : #inspecting }
DRControlFlowGraph >> createBlockView: aBlock [
	| box label boxes composite labelBackground |
	label := RSLabel new 
		text: aBlock id asString;
		color: Color black;
		fontSize: 4;
		yourself.
	labelBackground := RSEllipse new
		color: Color white;
		border: (RSBorder new color: 'black');
		extent: label extent + 5;
		yourself.
	box := RSBox new
		noPaint;
		border: (RSBorder new 
			width: 2; 
			color: Color black;
			yourself);
		cornerRadius: 2;
		yourself.

	boxes := aBlock instructions collect: [ :inst | 
		RSBox new
			model: inst;
			color: (self instructionColor: inst);
			border: (RSBorder new color: Color black; joinMiter);
			yourself ] as: RSGroup.
	RSGridLayout on: boxes.
	box 
		position: boxes position;
		extent: boxes extent + 10.
	composite := ({box},boxes) asShape
		draggable;
		@ (RSPopup text: [:model | 
			String streamContents: [:s | (model instructions collect: #prettyPrint)
				asStringOn: s delimiter: String cr] 
			]);
		@ (RSMenuActivable new menuDo: [ :menu :element | 
			menu
				add: 'Tail-duplicate'
				target: element model
				selector: #tailDuplicate ]);
		model: aBlock;
		yourself.
	
	RSLocation new outer; right; top; offset: 3@(-3); move: label on: boxes.
	labelBackground position: label position.
	composite add: labelBackground.
	composite add: label.
	^ composite
]

{ #category : #'dead-path' }
DRControlFlowGraph >> deadBranches [

	^ self deadPaths select: [ :path | self isDeadBranch: path first source to: path first destination ]
]

{ #category : #edges }
DRControlFlowGraph >> deadPaths [

	| paths |
	deadPaths ifNotNil: [ ^ deadPaths ].
	paths := OrderedDictionary new.
	edges select: [ :e | e isDead ] thenDo: [ :e | 
		paths
			at: e deadTarget
			ifPresent: [ :collection | collection add: e ]
			ifAbsentPut: [{ e } asOrderedCollection] ].
	^ deadPaths := paths
]

{ #category : #accessing }
DRControlFlowGraph >> edges [

	^ edges
]

{ #category : #edges }
DRControlFlowGraph >> edgesFrom: sourceBlock to: destinationBlock [

	^ (self outgoingEdgesFor: sourceBlock) select: [ :e | e destination = destinationBlock ]
]

{ #category : #accessing }
DRControlFlowGraph >> firstBasicBlock [

	^ initialBasicBlock successor
]

{ #category : #edges }
DRControlFlowGraph >> fixBackJumps [

	self blocks do: [ :block | 
		block successors do: [ :succ | 
			(block endInstruction isBackJumpTo: succ) ifTrue: [ "If back-jump to block a recursive phi should exist"
				(succ phiFunctions anySatisfy: [ :phi | phi hasRecursiveUse ]) 
					ifFalse: [ "If not, then is a forward-jump" 
						block endInstruction beForwardJumpTo: succ ] ] ].

		"Check if has a back-jump to this block"
		(block phiFunctions anySatisfy: [ :phi | phi hasRecursiveUse ]) 
			ifTrue: [ 
				block backJumpPredecessors do: [ :pred | 
					pred endInstruction beBackJumpTo: block ] ] ]
]

{ #category : #edges }
DRControlFlowGraph >> generatePaths [

	self clearEdges.
	(DRPathFactory withCFG: self) generatePaths
]

{ #category : #edges }
DRControlFlowGraph >> incomingEdgesFor: aBlock [

	^ edges select: [ :e | e destination = aBlock ]
]

{ #category : #spilling }
DRControlFlowGraph >> incrementSpillSlots [
	
	^ numberOfSpillSlots := numberOfSpillSlots + 1
]

{ #category : #accessing }
DRControlFlowGraph >> initialBasicBlock [ 
	^ initialBasicBlock
]

{ #category : #accessing }
DRControlFlowGraph >> initialConstraint: constraint [
	initialConstraint := constraint
]

{ #category : #accessing }
DRControlFlowGraph >> initialContrainst [

	^ initialConstraint
]

{ #category : #initialization }
DRControlFlowGraph >> initialize [

	super initialize.
	instructionFactory := DRInstructionFactory new.
	
	basicBlocks := OrderedCollection new.
	edges := OrderedCollection new.
	edgesBySource := Dictionary new.
	
	"How many spill slots we need in memory to execute the compiled method"
	numberOfSpillSlots := 0.
	
	nextBasicBlockId := 0.
	initialBasicBlock := self privateNewBasicBlock.
]

{ #category : #inspecting }
DRControlFlowGraph >> inspectionCFG [

	<inspectorPresentationOrder: 0 title: 'CFG'>
	| view |
	view := RSCanvas new.
	^ self showIn: view
]

{ #category : #accessing }
DRControlFlowGraph >> instructionByResultId: anInteger [ 

	^ self instructions detect: [ :i | i result hasId: anInteger ]
]

{ #category : #inspecting }
DRControlFlowGraph >> instructionColor: instruction [

	instruction isJump ifTrue: [ 
		instruction isBackJump ifTrue: [ ^ Color purple muchLighter ].
		instruction isBranchIfCondition ifTrue: [ ^ Color purple ].
		instruction isBranchIfTrue ifTrue: [ ^ Color purple ].
		^ Color purple muchDarker ].
	instruction isMandatoryInstruction ifTrue: [ ^ Color red ].
	instruction users isEmpty ifTrue: [ ^ Color gray ].
	instruction isPhiFunction ifTrue: [ ^ Color orange ].
	(instruction operands anySatisfy: [ :op | op isPhysicalRegister ]) 
		ifTrue: [ ^ Color red darker ].
	(instruction operands anySatisfy: [ :op | op isConstant ]) ifTrue: [ 
		^ Color yellow darker ].
	instruction isCopy ifTrue: [ ^ Color blue muchLighter ].

	^ Color white
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

{ #category : #testing }
DRControlFlowGraph >> isAlmostDeadBranch: aDRBasicBlock to: aDRBasicBlock2 [

	^ (self edgesFrom: aDRBasicBlock to: aDRBasicBlock2) anySatisfy: [ :e | 
		  e isDead ]
]

{ #category : #testing }
DRControlFlowGraph >> isDeadBranch: aDRBasicBlock to: aDRBasicBlock2 [

	^ (self edgesFrom: aDRBasicBlock to: aDRBasicBlock2) allSatisfy: [ :e | 
		  e isDead ]
]

{ #category : #accessing }
DRControlFlowGraph >> lastBasicBlock [

	^ self reversePostOrderBlocks last
]

{ #category : #accessing }
DRControlFlowGraph >> lastPaths [

	^ (self blocks
		   select: [ :b | b successors isEmpty ]
		   thenCollect: [ :b | self incomingEdgesFor: b ]) flattened
]

{ #category : #accessing }
DRControlFlowGraph >> leafBlocks [

	^ self blocks select: [ :b | b endInstruction isNoop ]
]

{ #category : #building }
DRControlFlowGraph >> newBasicBlock [
	
	^ self addBasicBlock: self privateNewBasicBlock
]

{ #category : #building }
DRControlFlowGraph >> newBasicBlockWith: aFullBlockClosure [ 
	
	| newBlock |
	newBlock := self newBasicBlock.
	aFullBlockClosure value: newBlock.
	^ newBlock
]

{ #category : #accessing }
DRControlFlowGraph >> numberOfSpillSlots [
	
	^ numberOfSpillSlots
]

{ #category : #edges }
DRControlFlowGraph >> outgoingEdgesFor: aBlock [

	^ edgesBySource at: aBlock ifAbsent: [ OrderedCollection new ]
]

{ #category : #accessing }
DRControlFlowGraph >> pathsSize [

	^ self lastPaths size
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

{ #category : #building }
DRControlFlowGraph >> privateNewBasicBlock [
	
	| newBlock |
	newBlock := DRBasicBlock id: self allocateBasicBlockID.
	newBlock controlFlowGraph: self.
	^ newBlock
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

{ #category : #inspecting }
DRControlFlowGraph >> showBranchConstraints: branchEdges [

	^ String streamContents: [ :s | 
		  branchEdges
			  do: [ :e | 
				  e constraints associationsDo: [ :assoc | 
					  s << (assoc key asString , ' ' , assoc value asString , String cr) ] ]
			  separatedBy: [ 
				  s << (String cr , '--o--o--o--o--o--o--o--' , String cr) ] ]
]

{ #category : #inspecting }
DRControlFlowGraph >> showIn: view [

	| blockViews controller |
	blockViews := self blocks collect: [ :b | self createBlockView: b. ].
	view addAll: blockViews.
	self showLines: blockViews in: view.
	self showInfoIn: view.
	
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

{ #category : #inspecting }
DRControlFlowGraph >> showInfoIn: view [

	| label instructions blocks paths |
	label := [ :text | 
	         RSLabel new
		         text: text;
		         color: Color black;
		         fontSize: 14;
		         bold;
		         yourself ].


	blocks := label value: self blocks size asString , ' blocks'.
	instructions := label value:
		                self instructions size asString , ' instructions'.
	paths := label value: self pathsSize asString , ' paths'.

	RSLocation new
		below;
		move: blocks on: view;
		move: instructions on: blocks;
		move: paths on: instructions.

	view addAll: { 
			blocks.
			instructions.
			paths }
]

{ #category : #inspecting }
DRControlFlowGraph >> showLines: blockViews in: view [

	| eb |
	eb := RSLineBuilder arrowedLine.
	eb canvas: view.
	eb color: Color blue muchDarker.
	eb width: 2.
	eb withBorderAttachPoint.
	eb shapes: blockViews.
	eb connectToAll: #successors.
	view lines pushBack.
	view lines do: [ :line | 
		| source destination |
		source := line model key.
		destination := line model value.
		line model: (self edgesFrom: source to: destination).
		line @ (RSPopup text: [ :edgs | self showBranchConstraints: edgs ]).
		(self isAlmostDeadBranch: source to: destination) ifTrue: [ 
			line color: Color green muchDarker ].
		(self isDeadBranch: source to: destination) ifTrue: [ 
			line color: Color red muchDarker ] ]
]

{ #category : #accessing }
DRControlFlowGraph >> tree [

	^ tree
]

{ #category : #accessing }
DRControlFlowGraph >> tree: anObject [

	tree := anObject
]

{ #category : #validating }
DRControlFlowGraph >> validate [

	self buildDominatorTree.
	self blocks do: [ :e | e validate ]
]
