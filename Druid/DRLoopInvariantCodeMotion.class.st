Class {
	#name : #DRLoopInvariantCodeMotion,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRLoopInvariantCodeMotion >> applyTo: aDRControlFlowGraph [

	"The current implementation of loop-invariant code motion has the following limitations:
	- it won't optimize loops that contain multiple basic blocks (e.g. loops that contain ifs)
	- when optimizing nested loops, it will only hoist loop-invariant instructions out of a single loop"

	aDRControlFlowGraph allBackJumps do: [ :backJump | 
		self applyToLoop: backJump ]
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> applyToLoop: backJump [

	| instructionsToMove |
	instructionsToMove := (backJump loopBlocks flatCollect: [ :block | 
		                       block instructions ]) select: [ :instruction | 
		                      instruction canMoveOutsideLoopWith: backJump ].
	instructionsToMove ifEmpty: [ ^ self ].

	self
		moveAllLoopInstructions: instructionsToMove
		outsideBackjump: backJump
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> moveAllLoopInstructions: instructionsToMove outsideBackjump: backJump [

	| loopHeaderBlocks loopHeaderBlock loopPredecessors loopPredecessor preheaderBlock preheaderConditionBlock conditionJump loopConditionJump loopExitBlock |
	loopHeaderBlocks := backJump backJumpTargets.
	self assert: [ loopHeaderBlocks size = 1 ].
	loopHeaderBlock := loopHeaderBlocks anyOne.

	loopPredecessors := loopHeaderBlock predecessors asSet copyWithout:
		                    backJump basicBlock.
	self assert: [ loopPredecessors size = 1 ].
	loopPredecessor := loopPredecessors anyOne.

	preheaderBlock := loopPredecessor newEmptySuccessors anyOne.
	preheaderConditionBlock := loopHeaderBlock basicDuplicate.
	"We need to replace the phis from last to first, since phis should otherwise be declares above other instructions"
	preheaderConditionBlock phiFunctions reverse do: [ :phi | 
		phi rewirePhiTo: preheaderBlock ].
	preheaderConditionBlock predecessors copy do: [ :p | 
		preheaderConditionBlock removePredecessor: p ].

	conditionJump := preheaderConditionBlock lastInstruction.
	loopConditionJump := loopHeaderBlock lastInstruction.
	loopExitBlock := loopConditionJump falseBranch.

	loopExitBlock addPredecessor: preheaderConditionBlock.
	conditionJump trueBranch: preheaderBlock.

	loopPredecessor
		replaceSuccessor: preheaderBlock
		by: preheaderConditionBlock.
	preheaderBlock
		replacePredecessor: loopPredecessor
		by: preheaderConditionBlock.
	preheaderConditionBlock addPredecessor: loopPredecessor.

	loopHeaderBlock
		mergeInstructionsWithDuplicatedBlock: preheaderConditionBlock
		withSuccessors: { loopExitBlock }
		usingUsers: [ :i | 
			i nonLocalUsers select: [ :u | 
				u isDominatedBy: loopExitBlock firstInstruction ] ].

	instructionsToMove do: [ :i | 
		i moveToDominatingBlock: preheaderBlock ].

]
