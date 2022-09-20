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
	instructionsToMove := (backJump loopBlocks flatCollect: [ :block | block instructions ]).
	self tryMoveAllLoopInstructions: instructionsToMove outsideBackjump: backJump
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> createLoopPreheaderBlocks: backJump [

	| preheaderBlock loopPredecessor conditionJump loopHeaderBlock loopExitBlock preheaderConditionBlock loopConditionJump |
	loopHeaderBlock := backJump backJumpTargets unique.
	loopPredecessor := (loopHeaderBlock predecessors asSet copyWithout:
		                    backJump basicBlock) unique.

	"1. Create preheader and preheader condition blocks"
	preheaderBlock := loopPredecessor newEmptySuccessors anyOne.
	preheaderConditionBlock := loopHeaderBlock basicDuplicate.

	"2. Rewire preheader condition block phis"
	"We need to replace the phis from last to first, since phis should otherwise be declares above other instructions"
	preheaderConditionBlock phiFunctions reverse do: [ :phi | 
		phi rewirePhiTo: preheaderBlock ].
	preheaderConditionBlock predecessors copy do: [ :p | 
		preheaderConditionBlock removePredecessor: p ].

	"3. Link new blocks"
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
	^ preheaderBlock
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> moveAllLoopInstructions: instructionsToMove outsideBackjump: backJump intoPreheader: preheader [

	| preheaderBlock |
	preheaderBlock := preheader ifNil: [ self createLoopPreheaderBlocks: backJump ].
	instructionsToMove do: [ :i | 
		i moveToDominatingBlock: preheaderBlock ]
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> tryMoveAllLoopInstructions: candidateInstructions outsideBackjump: backJump [

	self
		tryMoveAllLoopInstructions: candidateInstructions
		outsideBackjump: backJump
		intoPreheader: nil
]

{ #category : #operations }
DRLoopInvariantCodeMotion >> tryMoveAllLoopInstructions: candidateInstructions outsideBackjump: backJump intoPreheader: preheader [

	| instructionsToMove preheaderBlock headerBlock |
	instructionsToMove := candidateInstructions select: [ :instruction | 
		                      instruction canMoveOutsideLoopWith: backJump ].
	instructionsToMove ifEmpty: [ ^ self ].

	self
		moveAllLoopInstructions: instructionsToMove
		outsideBackjump: backJump
		intoPreheader: preheader.

	headerBlock := backJump basicBlock successor.
	preheaderBlock := preheader ifNil: [(headerBlock predecessors asSet copyWithout:
		                    backJump basicBlock) unique].

	self
		tryMoveAllLoopInstructions:
		(instructionsToMove flatCollect: [ :i | i users ])
		outsideBackjump: backJump
		intoPreheader: preheaderBlock
]
