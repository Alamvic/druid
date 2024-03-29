Class {
	#name : #DRDominatorTree,
	#superclass : #Object,
	#instVars : [
		'tree',
		'blockDictionary',
		'dominanceFrontiers'
	],
	#category : #'Druid-Optimizations'
}

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

{ #category : #'as yet unclassified' }
DRDominatorTree >> buildDominatorTreeOf: cfg [

	| hasChanged currentNode newImmediateDominator|
	self initialize: cfg.
	hasChanged := true.
	[ hasChanged ] whileTrue: [ 
		hasChanged := false.
		cfg blocksDo: [ :b | 
			currentNode := blockDictionary at: b.
			newImmediateDominator := currentNode findImmediateDominator: self.
			(currentNode parent = newImmediateDominator) ifFalse: [ 
				currentNode replaceParent: newImmediateDominator.
				hasChanged := true.
			]
		]]
]

{ #category : #'as yet unclassified' }
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

{ #category : #'as yet unclassified' }
DRDominatorTree >> dominanceFrontierOf: aBlock [

	^ self computeDominanceFrontier at: aBlock ifAbsent: [ OrderedCollection new ]
]

{ #category : #query }
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

{ #category : #asd }
DRDominatorTree >> immediateDominatorOf: aBlock [

	| node |
	node := blockDictionary at: aBlock.
	^ node parent block
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> initialize: cfg [

	| currentNode workList node |
	
	blockDictionary := Dictionary new.
	tree := DRDominatorTreeNode new.
	workList := OrderedCollection new.
	workList add: tree.
	
	tree block: cfg initialBasicBlock.
	blockDictionary at: cfg initialBasicBlock put: tree.
	
	[ workList isNotEmpty ] whileTrue: [
		currentNode := workList removeAt: 1.
		currentNode block endInstruction targets do: [ :b |
			node := DRDominatorTreeNode new.
			blockDictionary at: b ifAbsent: [
				blockDictionary add: b->node.
				workList addFirst: (node block: b;
				parent: currentNode;
				yourself).
				currentNode children addLast: node.
			].
		].
	]
	
	
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

	| frontier |
	frontier := self dominanceFrontierOf: aDRBasicBlock.
	^ frontier , (frontier flatCollect: [ :each | self iteratedDominanceFrontierOf: each ])
]

{ #category : #'as yet unclassified' }
DRDominatorTree >> minimalDominanceFrontierOfAll: blocks [

	| frontier |
	frontier := (blocks collect: [ :b | self dominanceFrontierOf: b ])
		            flattened asSet.

	^ frontier asOrderedCollection
]

{ #category : #'as yet unclassified' }
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
DRDominatorTree >> pathToRoot: node [
	| currentNode path |
	currentNode := node.
	path := OrderedCollection new.
	path addFirst: currentNode.
	[ currentNode = tree ] whileFalse: [ 
			currentNode := currentNode parent.
			path addFirst: currentNode.
	].
	^ path
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

{ #category : #accessing }
DRDominatorTree >> tree [
	^ tree
]
