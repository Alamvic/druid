Class {
	#name : #DRLoopInvariantCodeMotion,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRLoopInvariantCodeMotion >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph allBackJumps do: [ :backJump | 
		(backJump loopBlocks flatCollect: [ :block | block instructions ]) 
			do: [ :instruction | 
			self tryMove: instruction outsideLoopWith: backJump ] ]
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> tryMove: potentiallyLoopInvariantInstruction outsideLoopWith: backJump [

	| isInvariant loopPredecessors loopPredecessor preheaderBlock loopHeaderBlock loopHeaderBlocks loopExitBlock loopConditionJump preheaderConditionBlock conditionJump |
	isInvariant := potentiallyLoopInvariantInstruction canMoveOutsideLoopWith: backJump.

	isInvariant ifFalse: [ ^ self ].

	loopHeaderBlocks := backJump backJumpTarget.
	self assert: [ loopHeaderBlocks size = 1 ].
	loopHeaderBlock := loopHeaderBlocks anyOne.
	
	loopPredecessors := (loopHeaderBlock predecessors) asSet copyWithout: backJump basicBlock.
	self assert: [ loopPredecessors size = 1 ].
	loopPredecessor := loopPredecessors anyOne.
	
	preheaderBlock := loopPredecessor newEmptySuccessors anyOne.
	preheaderConditionBlock := loopHeaderBlock basicDuplicate.
	"We need to replace the phis from last to first, since phis should otherwise be declares above other instructions"
	preheaderConditionBlock phiFunctions reverse do: [ :phi | phi rewirePhiTo: preheaderBlock ].
	preheaderConditionBlock predecessors copy do: [ :p |  preheaderConditionBlock removePredecessor: p].
	
	conditionJump := preheaderConditionBlock lastInstruction.
	loopConditionJump := loopHeaderBlock lastInstruction.
	loopExitBlock := loopConditionJump falseBranch.

	loopExitBlock addPredecessor: preheaderConditionBlock.
	conditionJump trueBranch: preheaderBlock.
	
	loopPredecessor replaceSuccessor: preheaderBlock by: preheaderConditionBlock.
	preheaderBlock replacePredecessor: loopPredecessor by: preheaderConditionBlock.
	preheaderConditionBlock addPredecessor: loopPredecessor. 
	
	loopHeaderBlock mergeInstructionsWithNewBlock: preheaderConditionBlock withSuccessors: { loopExitBlock }.
	
	potentiallyLoopInvariantInstruction moveToDominatingBlock: preheaderBlock.

	potentiallyLoopInvariantInstruction users do: [ :instruction | 
		self tryMove: instruction outsideLoopWith: backJump ]
]
