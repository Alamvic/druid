Class {
	#name : #DRDeadBranchElimination,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRDeadBranchElimination >> canBeAppliedWithGeneratedPathsIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph deadBranches isNotEmpty
]

{ #category : #operations }
DRDeadBranchElimination >> doApplyWithGeneratedPaths: cfg [
	"Remove all dead branches in CFG"

	| deadBranches |
	deadBranches := cfg deadBranches
		                collect: [ :edge | edge source -> edge destination ]
		                as: Set.
	deadBranches do: [ :edge |
		self removeBranchFrom: edge key to: edge value ]
]

{ #category : #optimising }
DRDeadBranchElimination >> removeBranchFrom: sourceBasicBlock to: destinationBasicBlock [

	| newSucessors |
	newSucessors := sourceBasicBlock successors copyWithout: destinationBasicBlock.
	newSucessors
		ifEmpty: [ sourceBasicBlock removeFromCFGAndDisconnectSuccessors ]
		ifNotEmpty: [ 
			self assert: newSucessors size < 2. "A basic block has 2 successors max"
			sourceBasicBlock jumpTo: newSucessors first ]
]
