Class {
	#name : 'DRBreakCriticalEdges',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRBreakCriticalEdges >> applyTo: cfg [
	
	cfg allConditionalJumps do: [ :jump |
		jump targets do: [ :target |
			target predecessors size > 1 ifTrue: [ 
				target breakCriticalEdgeToPredecessor: jump basicBlock.
			]
		]
	].

	cfg
		invalidateDominatorTree;
		invalidatePostDominatorTree;
		fixBackJumps.
]
