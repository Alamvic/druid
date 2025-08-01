Class {
	#name : 'DRABCDConstraintSolver',
	#superclass : 'DRConstraintSolver',
	#instVars : [
		'nodes',
		'edges',
		'worklist',
		'cfg',
		'memo'
	],
	#category : 'Druid-IR-Paths',
	#package : 'Druid',
	#tag : 'IR-Paths'
}

{ #category : 'instance creation' }
DRABCDConstraintSolver class >> for: aDRControlFlowGraph [

	^ self new initializeFor: aDRControlFlowGraph
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> addEdgesBetweenConstants [

	((nodes select: #isNumberValue) asOrderedCollection sortedAs: #value)
		overlappingPairsDo: [ :c1 :c2 |
			edges add: {
					c1.
					c2.
					(c2 value - c1 value) } ]
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> buildGraph [

	cfg instructions reject: #isMandatoryInstruction thenDo: [ :i |
		nodes add: i.
		self processValue: i ].

	self addEdgesBetweenConstants
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> graph [

	^ AIGraphWeightedFixtureStructure new
		nodes: nodes;
		edges: edges;
		yourself.
]

{ #category : 'initialization' }
DRABCDConstraintSolver >> initializeFor: aDRControlFlowGraph [

	cfg := aDRControlFlowGraph.

	nodes := Set new.
	edges := Set new.
	
	memo := Dictionary new.
	
	self buildGraph.

]

{ #category : 'testing' }
DRABCDConstraintSolver >> isSatisfiable: aDRPiNode [
	"TODO - for unsupported constraints, return true
			- do we need different ABCD subclasses for upper/lower bounds checks?"

	| shortestPath minABCDDistanceToBeSatisfiable |
	minABCDDistanceToBeSatisfiable := aDRPiNode constraint
		                                  minABCDDistanceToBeSatisfiable
		                                  ifNil: [ ^ true ].

	shortestPath := AIShortestPathWithMaxAndMinNodes new.
	shortestPath nodes: nodes.
	shortestPath
		edges: edges
		from: #first
		to: #second
		weight: #third.

	shortestPath
		start: (self piNodesOriginalVariable:
					 aDRPiNode constraint constantValue asDRValue);
		isMaxNodePredicate: [ :n | n isPhiFunction ];
		memoDictionary: memo.
	shortestPath run.

	"Distance < 0 means the other side is a tautology => this side is unsatisfiable
	>= 0 means this side isn't tautology => this side is satisfiable"
	^ (shortestPath
		   distanceTo: aDRPiNode operand
		   isLessThan: minABCDDistanceToBeSatisfiable) = -1
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> piNodesOriginalVariable: aDRValue [

	| val |
	val := aDRValue.
	[ val isPiNode ] whileTrue: [ val := val operand ].
	^ val
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> processAdd: aDRValue [

	| ops |
	ops := aDRValue operands.
	ops first isNumberValue ifTrue: [ ops := ops reversed ].
	ops second isNumberValue ifFalse: [ ^ nil ].

	edges add: {
			ops first.
			aDRValue.
			ops second value }
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> processCopy: aDRValue [
	"Remember a PiNode is a Copy too"

	aDRValue operands do: [ :op |
		nodes add: op.
		edges add: {
				op.
				aDRValue.
				0 } ].

	aDRValue isPiNode ifTrue: [ self processPiNode: aDRValue ]
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> processPiNode: aDRPiNode [

	| constraint distance |
	constraint := aDRPiNode constraint.
	nodes add: constraint constantValue asDRValue.

	distance := constraint distanceForBuildingABCDGraph.
	distance ifNil: [ ^nil ].

	edges add: {
			constraint constantValue asDRValue. "According to the paper, the edge source should be the pi-node associated to the constraint,
															but I don't think there's a difference here, and this is easier"
			aDRPiNode.
			distance }.

]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> processSub: aDRValue [

	| ops |
	ops := aDRValue operands.
	ops second isNumberValue ifFalse: [ ^ nil ].

	edges add: {
			ops first.
			aDRValue.
			ops second value negated }
]

{ #category : 'building - graph' }
DRABCDConstraintSolver >> processValue: aDRValue [

	aDRValue isAdd ifTrue: [ ^ self processAdd: aDRValue ].

	aDRValue isSubtract ifTrue: [ ^ self processSub: aDRValue ].

	(aDRValue isCopy or: aDRValue isPhiFunction) ifTrue: [
		^ self processCopy: aDRValue ]
]
