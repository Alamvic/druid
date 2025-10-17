Class {
	#name : 'DRPiDeadBranchElimination',
	#superclass : 'DROptimisation',
	#instVars : [
		'worklist',
		'visited',
		'solver'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'analysis' }
DRPiDeadBranchElimination >> addSuccessorsToWorklist: aDRBasicBlock [

	| tail |
	tail := self lastOfDirectBranchFrom: aDRBasicBlock.
	tail successors
		collect: [ :succ | DRPiDBEEdge withSource: tail andDestination: succ ]
		thenDo: [ :succ |
			(visited includes: succ) ifFalse: [
				worklist add: succ.
				visited add: succ
			]
		]
]

{ #category : 'operations' }
DRPiDeadBranchElimination >> applyTo: cfg [

	| deadEdges |
	visited := Set new.
	worklist := OrderedCollection new.
	self addSuccessorsToWorklist: cfg initialBasicBlock.

	deadEdges := self computeDeadEdges.

	deadEdges ifNotEmpty: [
		self removeUnnecessaryConditionalJumps: deadEdges.
		cfg
			invalidateDominatorTree;
			invalidatePostDominatorTree.
		DRDeadBlockElimination new applyTo: cfg]
]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> computeDeadEdges [

	| deadEdges |
	deadEdges := OrderedCollection new.

	[ worklist isEmpty ] whileFalse: [
		| currentEdge |
		currentEdge := worklist removeFirst.
		(self isAlive: currentEdge)
			ifTrue: [ self addSuccessorsToWorklist: currentEdge destination ]
			ifFalse: [ deadEdges add: currentEdge ] ].
		"TODO if a block is unreachable, then its sibling must be reachable,
		so we can remove it from the worklist"
	^ deadEdges
]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> directSuccessorsOf: aDRBasicBlock do: aBlock [

	| current |
	current := aDRBasicBlock.

	[
		aBlock value: current.
		current successors size = 1
			ifTrue: [
				current := current successors unique.
				true ]
			ifFalse: [ false ]
	] whileTrue
]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> isAlive: aDREdge [

	^ self isReachable: aDREdge destination from: aDREdge source.

]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> isReachable: destinationBlock from: sourceBlock [

	| phiReplacements i |
	
	phiReplacements := Dictionary new.
	i := destinationBlock predecessors indexOf: sourceBlock.
	destinationBlock phiFunctions do: [ :phi |
		phiReplacements at: phi put: (phi operands at: i).
	].
	
	^ (self piNodesOfBranchWithHeader: destinationBlock) allSatisfy: [ :pi |
		solver isSatisfiable: pi replacingPhis: phiReplacements ]
]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> lastOfDirectBranchFrom: aDRBasicBlock [

	| tail |
	
	tail := aDRBasicBlock.
	self directSuccessorsOf: aDRBasicBlock do: [ :b | tail := b ].
	^ tail
]

{ #category : 'analysis' }
DRPiDeadBranchElimination >> piNodesOfBranchWithHeader: aDRBasicBlock [

	| piNodes |
	
	piNodes := OrderedCollection new.
	self
		directSuccessorsOf: aDRBasicBlock
		do: [ :b | piNodes addAll: b piNodes ].
	^ piNodes
]

{ #category : 'removing' }
DRPiDeadBranchElimination >> removeUnnecessaryConditionalJumps: deadEdges [

	deadEdges do: #kill
]
