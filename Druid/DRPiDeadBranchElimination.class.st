Class {
	#name : 'DRPiDeadBranchElimination',
	#superclass : 'DROptimisation',
	#instVars : [
		'worklist',
		'visited'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiDeadBranchElimination >> applyTo: cfg [

	| deadEdges |
	worklist add: (DREdge
			 withSource: cfg initialBasicBlock
			 andDestination: cfg initialBasicBlock).
	"The source doesn't matter in the initial edge, it'll be never be dead anyways"

	deadEdges := OrderedCollection new.

	[ worklist isEmpty ] whileFalse: [
		| currentEdge |
		currentEdge := worklist removeFirst.
		(self isReachable: currentEdge destination)
			ifTrue: [ self updateWorklistFrom: currentEdge destination ]
			ifFalse: [ deadEdges add: currentEdge ] ].

	deadEdges ifNotEmpty: [
		self removeUnnecessaryConditionalJumps: deadEdges.
		cfg applyOptimisation: DRDeadBlockElimination new ].

]

{ #category : 'initialization' }
DRPiDeadBranchElimination >> initialize [

	worklist := OrderedCollection new.
	visited := Set new.
]

{ #category : 'testing' }
DRPiDeadBranchElimination >> isReachable: aDRBasicBlock [

	aDRBasicBlock piNodes ifEmpty: [ ^true ].

	^ aDRBasicBlock piNodes allSatisfy: [ :pi | pi constraint isSatisfiableFor: pi operand ].

]

{ #category : 'removing' }
DRPiDeadBranchElimination >> removeUnnecessaryConditionalJumps: deadEdges [ 

	deadEdges do: [ :edge | edge kill ].
]

{ #category : 'as yet unclassified' }
DRPiDeadBranchElimination >> updateWorklistFrom: aDRBasicBlock [

	aDRBasicBlock successors
		select: [ :succ | (visited includes: succ) not ]
		thenDo: [ :succ |
			worklist add: (DREdge withSource: aDRBasicBlock andDestination: succ).
			visited add: succ.
		].
]
