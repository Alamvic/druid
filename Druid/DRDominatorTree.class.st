Class {
	#name : #DRDominatorTree,
	#superclass : #Object,
	#instVars : [
		'blockDictionary',
		'dominanceFrontiers',
		'root'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #querying }
DRDominatorTree >> allChildrenOf: aBlock [

	^ (blockDictionary at: aBlock) allChildren collect: [ :e | e block ]
]

{ #category : #inspection }
DRDominatorTree >> asInspectorIn: view [

	self configureVisualization: view.
	^ SpRoassal3InspectorPresenter new
		canvas: view;
		yourself
]

{ #category : #accessing }
DRDominatorTree >> blockDictionary [
	^ blockDictionary
]

{ #category : #initialization }
DRDominatorTree >> buildDominationTreeOf: cfg startingFrom: initialBasicBlock followingInitially: initializationBlockClosure thenFollowing: dominationBlockClosure [

	| hasChanged newImmediateDominator |
	self
		initialize: cfg
		startingFrom: initialBasicBlock
		iterating: initializationBlockClosure.
	hasChanged := true.
	[ hasChanged ] whileTrue: [
		hasChanged := false.
		({
			 cfg initialBasicBlock.
			 cfg exitBasicBlock } , cfg blocks copyWithout: initialBasicBlock)
			do: [ :block |
				blockDictionary at: block ifPresent: [ :currentNode |
					newImmediateDominator := self findImmediateDominatorOfBlock:
						                         (dominationBlockClosure value: block).
					currentNode parent = newImmediateDominator ifFalse: [
						currentNode replaceParent: newImmediateDominator.
						hasChanged := true ] ] ] ]
]

{ #category : #initialization }
DRDominatorTree >> buildDominatorTreeOf: cfg [

	self
		buildDominationTreeOf: cfg
		startingFrom: cfg initialBasicBlock
		followingInitially: [ :aBlock | aBlock successors ]
		thenFollowing: [ :aBlock | aBlock predecessors ]
]

{ #category : #initialization }
DRDominatorTree >> buildPostDominatorTreeOf: cfg [

	cfg exitBasicBlock ifNil: [
		self error:
			'We need an exitBasicBlock to build a post dominator tree' ].
	self
		buildDominationTreeOf: cfg
		startingFrom: cfg exitBasicBlock
		followingInitially: [ :aBlock | aBlock predecessors ]
		thenFollowing: [ :aBlock | aBlock successors ]
]

{ #category : #querying }
DRDominatorTree >> childrenOf: aBlock [

	^ (blockDictionary at: aBlock) children collect: [ :e | e block ]
]

{ #category : #initialization }
DRDominatorTree >> computeDominanceFrontier [

	dominanceFrontiers ifNotNil: [ ^ dominanceFrontiers ].

	dominanceFrontiers := Dictionary new.
	blockDictionary keys do: [ :b | 
		b predecessors size > 1 ifTrue: [ 
			b predecessors do: [ :p | 
				| runner previousFrontier |
				runner := p.
				[ runner ~= (self immediateDominatorOf: b) ] whileTrue: [ 
					previousFrontier := dominanceFrontiers
						                    at: runner
						                    ifAbsentPut: [ OrderedCollection new ].
					previousFrontier add: b.
					runner := self immediateDominatorOf: runner ] ] ] ].
	^ dominanceFrontiers
]

{ #category : #inspection }
DRDominatorTree >> configureVisualization: view [

	| blockViews controller |
	blockViews := self blockDictionary values collect: [ :treeNode | self createBlockView: treeNode ].

	view addAll: blockViews.
	self showLines: blockViews in: view.

	"All the characters are displayed using a grid layout"
	RSSugiyamaLayout new
		horizontalGap: 50;
		applyOn: blockViews.

	controller := RSCanvasController new
		noLegend;
		yourself.

	view @ controller
]

{ #category : #inspection }
DRDominatorTree >> createBlockView: treeNode [

	| box label composite labelBackground |
	label := RSLabel new
		         text: treeNode block id asString;
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

	"	box
		position: boxes position;
		extent: boxes extent + 10."
	composite := { box } asShape
		             draggable;
		             model: treeNode;
		             yourself. ",boxes"

	RSLocation new
		outer;
		right;
		top;
		offset: 3 @ -3. ";
		move: label on: boxes"

	labelBackground position: label position.
	composite add: labelBackground.
	composite add: label.
	^ composite
]

{ #category : #querying }
DRDominatorTree >> dominanceFrontierOf: aBlock [

	^ self computeDominanceFrontier at: aBlock ifAbsent: [ OrderedCollection new ]
]

{ #category : #querying }
DRDominatorTree >> dominatorOfAll: instructions [

	| firstNode |
	self assert: instructions size > 1.

	firstNode := self blockDictionary at: instructions first basicBlock.

	^ instructions allButFirst
		  inject: firstNode
		  into: [ :dominator :instruction |
			  self
				  nearestCommonAncestorOf: dominator
				  and: (self blockDictionary at: instruction basicBlock) ]
]

{ #category : #initialization }
DRDominatorTree >> findImmediateDominatorOfBlock: nextBlocksToIterate [
	"Method used during the tree construction, don't use it outside of it"

	| nearestCommonAncestor currentBlock |
	nearestCommonAncestor := self blockDictionary at:
		                         nextBlocksToIterate first.
	nextBlocksToIterate do: [ :p |
		currentBlock := self blockDictionary at: p.
		nearestCommonAncestor := self
			                         nearestCommonAncestorOf:
			                         nearestCommonAncestor
			                         and: currentBlock ].
	^ nearestCommonAncestor
]

{ #category : #querying }
DRDominatorTree >> immediateDominatorOf: aBlock [

	| node |
	node := blockDictionary at: aBlock.
	^ node parent block
]

{ #category : #initialization }
DRDominatorTree >> initialize: cfg startingFrom: aBlockClosure [

	| currentNode workList node initialBasicBlock |
	blockDictionary := Dictionary new.
	root := DRDominatorTreeNode new.
	workList := OrderedCollection new.
	workList add: root.

	initialBasicBlock := aBlockClosure value: cfg.
	root block: initialBasicBlock.
	blockDictionary at: initialBasicBlock put: root.

	[ workList isNotEmpty ] whileTrue: [
		currentNode := workList removeFirst.
		currentNode block successors do: [ :b |
			blockDictionary at: b ifAbsent: [
				node := DRDominatorTreeNode new
					        block: b;
					        parent: currentNode;
					        yourself.
				currentNode addChild: node.

				blockDictionary at: b put: node.
				workList addFirst: node ] ] ]
]

{ #category : #initialization }
DRDominatorTree >> initialize: cfg startingFrom: initialBasicBlock iterating: aBlockClosure [

	| currentNode workList node |
	blockDictionary := Dictionary new.
	root := DRDominatorTreeNode new.
	workList := OrderedCollection new.
	workList add: root.

	root block: initialBasicBlock.
	blockDictionary at: initialBasicBlock put: root.

	[ workList isNotEmpty ] whileTrue: [
		currentNode := workList removeFirst.
		(aBlockClosure value: currentNode block) do: [ :b |
			blockDictionary at: b ifAbsent: [
				node := DRDominatorTreeNode new
					        block: b;
					        parent: currentNode;
					        yourself.
				currentNode addChild: node.

				blockDictionary at: b put: node.
				workList addFirst: node ] ] ]
]

{ #category : #inspection }
DRDominatorTree >> inspectionDominatorTree [

	<inspectorPresentationOrder: 0 title: 'DominatorTree'>
	| view |
	view := RSCanvas new.
	^ self asInspectorIn: view
]

{ #category : #testing }
DRDominatorTree >> isDominator: dominatorBlock of: dominatedBlock [
	
	| node |
	node := blockDictionary at: dominatedBlock.
	^ node isDominatedByBlock: dominatorBlock.
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> iteratedDominanceFrontierOf: aDRBasicBlock [
	" DF+(n) = DF(n) U DF+(DF(n)) 
		  returns a set of Dominance Frontier of the block "

	| frontier frontierWithoutLoops |
	frontier := self dominanceFrontierOf: aDRBasicBlock.
	frontierWithoutLoops := frontier copyWithout: aDRBasicBlock.
	^ frontier , (frontierWithoutLoops flatCollect: [ :each | self iteratedDominanceFrontierOf: each ])
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> iteratedDominanceFrontierOfAll: aCollection [
	"returns a set witht the union of Dominance Frontiers of all blocks passed as argument"

	^ aCollection flatCollectAsSet: [ :block |
		  self iteratedDominanceFrontierOf: block ]
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> lookup: aBlock startingAt: aBasicBlock [
	"Look up in the dominator tree starting from a node using a block.
	If the block returns true, return the basic block.
	If the block return false continue in the parent.
	Throw an exception when above the root"

	self pathToRootBasicBlock: aBasicBlock do: [ :blockInPath |
		(aBlock value: blockInPath) ifTrue: [ ^ blockInPath ] ].
	NotFound signal: aBlock
]

{ #category : #querying }
DRDominatorTree >> minimalDominanceFrontierOfAll: blocks [

	| frontier |
	frontier := (blocks collect: [ :b | self dominanceFrontierOf: b ])
		            flattened asSet.

	^ frontier asOrderedCollection
]

{ #category : #querying }
DRDominatorTree >> nearestCommonAncestorOf: n1 and: n2 [
	| n1Path n2Path minPathLength |
	n1Path := self pathToRoot: n1.
	n2Path := self pathToRoot: n2.
	minPathLength := n1Path size min: n2Path size.
	1 to: minPathLength do: [ :i |
		(n1Path at: i) = (n2Path at: i) ifFalse: [ ^ n1Path at: i - 1 ].
	].
	^ n1Path at: minPathLength.
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> nodeOfBlock: aBasicBlock [

	^ self blockDictionary at: aBasicBlock
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> pathToRoot: node [

	| path |
	path := OrderedCollection new.

	self
		pathToRoot: node
		do: [ :currentNode | path addFirst: currentNode ].

	^ path
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> pathToRoot: node do: aBlock [

	| currentNode |
	currentNode := node.
	aBlock value: currentNode.
	[ currentNode = tree ] whileFalse: [
		currentNode := currentNode parent.
		aBlock value: currentNode ]
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> pathToRootBasicBlock: aBasicBlock do: aBlock [
	"Iterate the tree from the given basic block to the root executing the block for each block in the path"

	self
		pathToRootNode: (self nodeOfBlock: aBasicBlock)
		do: [ :node | aBlock value: node block ]
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> pathToRootNode: node do: aBlock [

	"Iterate the tree from the given node to the root executing the block for each node in the path"
	
	| currentNode |
	currentNode := node.
	aBlock value: currentNode.
	[ currentNode = tree ] whileFalse: [
		currentNode := currentNode parent.
		aBlock value: currentNode ]
]

{ #category : #accessing }
DRDominatorTree >> root [

	^ root
]

{ #category : #inspection }
DRDominatorTree >> showLines: blockViews in: view [

	| eb |
	eb := RSLineBuilder arrowedLine.
	eb canvas: view.
	eb color: Color blue muchDarker.
	eb width: 2.
	eb withBorderAttachPoint.
	eb shapes: blockViews.
	eb connectToAll: [ :e | e children ].

	view lines pushBack.
	view lines do: [ :line |
		| source destination |
		source := line model key.
		destination := line model value ]
]
