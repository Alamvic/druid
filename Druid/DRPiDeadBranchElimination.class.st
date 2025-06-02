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
		reject: [ :succ | visited includes: succ ]
		thenDo: [ :succ |
			worklist add: succ.
			visited add: succ.
		].
]

{ #category : 'accessing' }
DRPiDeadBranchElimination >> applyTo: cfg [

	| unreachableBlocks |
	visited := Set new.
	worklist := OrderedCollection with: cfg initialBasicBlock.

	unreachableBlocks := self computeUnreachableBlocks.

	unreachableBlocks ifNotEmpty: [
		self removeUnnecessaryConditionalJumps: unreachableBlocks.
		cfg
			invalidateDominatorTree;
			invalidatePostDominatorTree.
		cfg applyOptimisation: DRDeadBlockElimination new ]
]

{ #category : 'accessing' }
DRPiDeadBranchElimination >> computeUnreachableBlocks [

	| unreachableBlocks |
	unreachableBlocks := OrderedCollection new.

	[ worklist isEmpty ] whileFalse: [
		| currentBlock |
		currentBlock := worklist removeFirst.
		(self isReachable: currentBlock)
			ifTrue: [ self addSuccessorsToWorklist: currentBlock ]
			ifFalse: [ unreachableBlocks add: currentBlock ] ].
		"TODO if a block is unreachable, then its sibling must be reachable,
		so we can remove it from the worklist"
	^ unreachableBlocks
]

{ #category : 'as yet unclassified' }
DRPiDeadBranchElimination >> isReachable: aDRBasicBlock [

	^ aDRBasicBlock piNodes allSatisfy: [ :pi | solver isSatisfiable: pi ]
]

{ #category : 'removing' }
DRPiDeadBranchElimination >> removeUnnecessaryConditionalJumps: unreachableBlocks [

	unreachableBlocks do: [ :block |
		(DREdge withSource: block predecessors unique andDestination: block)
			kill ]
]
