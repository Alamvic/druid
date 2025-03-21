Class {
	#name : 'DRDeadEdgeSplitting',
	#superclass : 'DRPathOptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'testing' }
DRDeadEdgeSplitting >> canBeAppliedWithGeneratedPathsIn: aDRControlFlowGraph [ 

	^ aDRControlFlowGraph deadPaths isNotEmpty 
]

{ #category : 'accessing' }
DRDeadEdgeSplitting >> doApplyWithGeneratedPaths: aCFG [
	"Find the longest (not backjump) dead path in CFG and split guilty edge"

	| longestDeadPathEdge forwardEdges |
	"Avoid backjumps edges, they are difficult to fix"
	forwardEdges := aCFG deadPaths associations reject: [ :assoc | assoc key isBackJump ].
	forwardEdges ifEmpty: [ ^ self ].

	longestDeadPathEdge := (forwardEdges detectMax: [ :assoc | assoc value size ]) key.
	[ longestDeadPathEdge destination predecessors size = 1 ] whileTrue: [ 
		"If it is not splitteable, look the ancestors until splitting"
		longestDeadPathEdge := longestDeadPathEdge parent.
		longestDeadPathEdge ifNil: [ ^ self ].
		longestDeadPathEdge isBackJump ifTrue: [ ^ self ] ].

	longestDeadPathEdge split
]
