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

{ #category : 'as yet unclassified' }
DRPiDeadBranchElimination >> addSuccessorsToWorklist: aDRBasicBlock [

	aDRBasicBlock successors
		select: [ :succ | (visited includes: succ) not ]
		thenDo: [ :succ |
			worklist add: (DREdge withSource: aDRBasicBlock andDestination: succ).
			visited add: succ.
		].
]

{ #category : 'accessing' }
DRPiDeadBranchElimination >> applyTo: cfg [

	| deadEdges |
	visited := Set new.
	worklist := OrderedCollection with: (DREdge
			             withSource: cfg initialBasicBlock
			             andDestination: cfg initialBasicBlock).
	"The source doesn't matter in the initial edge, it'll be never be dead anyways"

	deadEdges := self computeDeadeEdges.

	deadEdges ifNotEmpty: [
		self removeUnnecessaryConditionalJumps: deadEdges.
		cfg
			invalidateDominatorTree;
			invalidatePostDominatorTree.
		cfg applyOptimisation: DRDeadBlockElimination new ]
]

{ #category : 'accessing' }
DRPiDeadBranchElimination >> computeDeadeEdges [

	| deadEdges |
	deadEdges := OrderedCollection new.

	[ worklist isEmpty ] whileFalse: [
		| currentEdge |
		currentEdge := worklist removeFirst.
		(self isReachable: currentEdge destination)
			ifTrue: [ self addSuccessorsToWorklist: currentEdge destination ]
			ifFalse: [ deadEdges add: currentEdge ] ].
		"TODO if a block is unreachable, then its sibling must be reachable,
		so we can remove it from the worklist"
	^ deadEdges
]

{ #category : 'as yet unclassified' }
DRPiDeadBranchElimination >> isReachable: aDRBasicBlock [

	^ aDRBasicBlock piNodes allSatisfy: [ :pi | solver isSatisfiable: pi ]
]

{ #category : 'removing' }
DRPiDeadBranchElimination >> removeUnnecessaryConditionalJumps: deadEdges [ 

	deadEdges do: [ :edge | edge kill ].
]
