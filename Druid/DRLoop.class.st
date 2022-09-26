"
I represent a loop inside Druid's control flow graph.

I'm not a node in the graph, but rather an utility used to access and manipulate loops inside CFGs.
"
Class {
	#name : #DRLoop,
	#superclass : #Object,
	#instVars : [
		'backJump'
	],
	#category : #'Druid-IR'
}

{ #category : #'instance creation' }
DRLoop class >> fromBackJump: aBackJump [ 
	| instance |
	instance := self new.
	instance backJump: aBackJump.
	^ instance
]

{ #category : #accessing }
DRLoop >> backJump [

	^ backJump
]

{ #category : #accessing }
DRLoop >> backJump: aBackJump [

	backJump := aBackJump
]

{ #category : #accessing }
DRLoop >> bodyBlocks [

	| loopGraphBlocks |
	loopGraphBlocks := self loopGraph blocks.
	"The loop graph contains copies of the complete graph's blocks. Return the original blocks"
	^ self controlFlowGraph blocks select: [ :b | 
		  b id ~= self headerBlock id and: [ 
			  loopGraphBlocks anySatisfy: [ :loopBlock | b id = loopBlock id ] ] ]
]

{ #category : #accessing }
DRLoop >> bodyInstructions [

	^ self bodyBlocks flatCollect: [ :block | block instructions ]
]

{ #category : #querying }
DRLoop >> containsStraightLineCode [

	^ self bodyBlocks size = 1
]

{ #category : #accessing }
DRLoop >> controlFlowGraph [

	^ self backJump basicBlock controlFlowGraph
]

{ #category : #'cfg manipulation' }
DRLoop >> createLoopPreheaderBlock [

	| loopPredecessor preheaderBlock |
	loopPredecessor := self preheaderBlock.
	preheaderBlock := loopPredecessor newEmptySuccessors anyOne.
	^ preheaderBlock
]

{ #category : #'cfg manipulation' }
DRLoop >> createLoopPreheaderBlockAndWrapWithIf [

	"The loop pre-header will contain the loop-invariant instructions that are moved out of the loop.
	The if is necessary to preserve semantics in case the loop body is never executed"

	| preheaderBlock loopPredecessor conditionJump headerBlock loopExitBlock preheaderConditionBlock loopConditionJump |
	preheaderBlock := self createLoopPreheaderBlock.
	headerBlock := self headerBlock.
	loopPredecessor := preheaderBlock predecessor.

	"1. Create preheader condition block"
	preheaderConditionBlock := headerBlock basicDuplicate.

	"2. Rewire preheader condition block phis"
	"We need to replace the phis from last to first, since phis should otherwise be declares above other instructions"
	preheaderConditionBlock phiFunctions reverse do: [ :phi | 
		phi rewirePhiTo: preheaderBlock ].
	preheaderConditionBlock predecessors copy do: [ :p | 
		preheaderConditionBlock removePredecessor: p ].

	"3. Link new blocks"
	conditionJump := preheaderConditionBlock lastInstruction.
	loopConditionJump := headerBlock lastInstruction.
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

	headerBlock
		mergeInstructionsWithDuplicatedBlock: preheaderConditionBlock
		withSuccessors: { loopExitBlock }
		usingUsers: [ :i | 
			i nonLocalUsers select: [ :u | 
				u isDominatedBy: loopExitBlock firstInstruction ] ].
	^ preheaderBlock
]

{ #category : #accessing }
DRLoop >> headerBlock [

	"A loop's header block is a basic block that contains the loop's condition"

	^ self latchBlock successor.
]

{ #category : #accessing }
DRLoop >> latchBlock [

	"A loop's latch block is a basic block that contains a back jump to the header of the loop"

	^ self backJump basicBlock
]

{ #category : #accessing }
DRLoop >> loopGraph [

	^ self controlFlowGraph
		  subgraphBetween: self headerBlock and: self latchBlock
]

{ #category : #accessing }
DRLoop >> preheaderBlock [

	"A loop's pre-header block is the basic block from which the loop is entered.
	There's always a single one if a pre-header block was created using #createLoopPreheaderBlocks,
	but if it was not, this method may fail."

	^ (self headerBlock predecessors asSet copyWithout: self latchBlock) unique
]

{ #category : #operations }
DRLoop >> unroll: unrollingFactor [

	| bodyBlock blockCopy loopPhiFunctions oldLastBlock newLastBlock |
	self containsStraightLineCode ifFalse: [ ^ self ].
	bodyBlock := self bodyBlocks anyOne.
	1 to: unrollingFactor - 1 do: [ :_ | 
		loopPhiFunctions := self headerBlock phiFunctions select: [ :phi | bodyBlock instructions anySatisfy: [ :result | phi operands includes: result] ].
		blockCopy := bodyBlock basicDuplicate.
		1halt.
		oldLastBlock := self bodyBlocks last.
		newLastBlock := blockCopy.
		oldLastBlock replaceSuccessor: self headerBlock by: newLastBlock.
		self backJump: self headerBlock endInstruction.
		self headerBlock replacePredecessor: oldLastBlock by: newLastBlock.
		newLastBlock replacePredecessor: self headerBlock by: oldLastBlock.
		self controlFlowGraph fixBackJumps.
		loopPhiFunctions do: [ :phi | | loopInput loopOutput newBlockInput newBlockOutput |
			loopInput := bodyBlock instructions detect: [:i | i hasDependency: phi ].
			loopOutput := bodyBlock instructions detect: [ :i | phi hasDependency: i ].
			newBlockInput := blockCopy instructions at: (bodyBlock instructions indexOf: loopInput).
			newBlockOutput := blockCopy instructions at: (bodyBlock instructions indexOf: loopInput).
			newBlockInput replaceDependency: phi by: loopOutput.
			phi replaceDependency: loopOutput by: newBlockOutput ] ]
]
