Class {
	#name : 'DRUnconditionalBackJumpScheluder',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRUnconditionalBackJumpScheluder >> applyTo: aCFG [

	aCFG allBackJumps
		select: [ :jump | jump isConditionalBranch ]
		thenDo: [ :jump | self transformToUnconditionalBackjump: jump ]
]

{ #category : 'transformation' }
DRUnconditionalBackJumpScheluder >> transformToUnconditionalBackjump: aDRBranchIfCondition [

	| cfg |
	cfg := aDRBranchIfCondition controlFlowGraph.
	aDRBranchIfCondition backJumpTargets do: [ :target | | newBlock |
		newBlock := cfg newBasicBlockWith: [ :b | b backJumpTo: target ].
		aDRBranchIfCondition replaceTarget: target by: newBlock.
		target removePredecessor: aDRBranchIfCondition basicBlock.
		newBlock addPredecessor: aDRBranchIfCondition basicBlock ]
]
