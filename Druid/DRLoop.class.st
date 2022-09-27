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
DRLoop >> blocks [

	| loopGraphBlocks |
	loopGraphBlocks := self loopGraph blocks.
	"The loop graph contains copies of the complete graph's blocks. Return the original blocks"
	^ self controlFlowGraph blocks select: [ :b | 
		  loopGraphBlocks anySatisfy: [ :loopBlock | b id = loopBlock id ] ]
]

{ #category : #accessing }
DRLoop >> bodyBlocks [

	^ self blocks select: [ :b | b ~= self headerBlock ]
]

{ #category : #accessing }
DRLoop >> bodyHyperblock [

	^ DRHyperBlock blocks: self bodyBlocks.
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

{ #category : #'cfg manipulation' }
DRLoop >> fixLoopPhiFunctions [

	| loopBody loopPhiFunctions |
	loopBody := self bodyHyperblock.
	loopPhiFunctions := self headerBlock phiFunctions select: [ :phi | 
		                    loopBody instructions anySatisfy: [ :result | 
			                    phi operands includes: result ] ].
	loopPhiFunctions do: [ :phi | 
		| phiUsers loopOutput newBlockInput newBlockOutput |
		phiUsers := self bodyInstructions select: [ :i | 
			            i hasDependency: phi ].
		"The first phi user is the actual loop input"
		newBlockInput := phiUsers second.
		loopOutput := self bodyInstructions detect: [ :i | 
			              phi hasDependency: i ].
		newBlockOutput := self bodyInstructions detect: [ :i | 
			                  i result isNoResult not and: [ 
				                  i users noneSatisfy: [ :u | u hasDependency: i ] ] ].
		newBlockInput clearUsers.
		newBlockInput replaceDependency: phi by: loopOutput.
		phi replaceDependency: loopOutput by: newBlockOutput ]
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

	| loopBody |
	loopBody := self bodyHyperblock.
	1 to: unrollingFactor - 1 do: [ :_ | 
		| bodyCopy oldLastBlock headerBlock |
		bodyCopy := DRHyperBlock blocks: loopBody duplicate values.
		oldLastBlock := self bodyHyperblock exitBlock.
		headerBlock := self headerBlock.

		bodyCopy removePredecessor: headerBlock.
		bodyCopy addBetween: oldLastBlock and: headerBlock.
		self backJump: bodyCopy exitBlock endInstruction.

		self fixLoopPhiFunctions ]
]
