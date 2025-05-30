"
I represent a loop inside Druid's control flow graph.

I'm not a node in the graph, but rather an utility used to access and manipulate loops inside CFGs.
"
Class {
	#name : 'DRLoop',
	#superclass : 'Object',
	#instVars : [
		'backJump'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRLoop class >> fromBackJump: aBackJump [ 
	| instance |
	instance := self new.
	instance backJump: aBackJump.
	^ instance
]

{ #category : 'accessing' }
DRLoop >> backJump [

	^ backJump
]

{ #category : 'accessing' }
DRLoop >> backJump: aBackJump [

	backJump := aBackJump
]

{ #category : 'accessing' }
DRLoop >> blocks [

	| loopGraphBlocks |
	loopGraphBlocks := self controlFlowGraph
		                   blocksBetween: self headerBlock
		                   and: self latchBlock.
	"The loop graph contains copies of the complete graph's blocks. Return the original blocks"
	^ self controlFlowGraph blocks select: [ :b |
		  loopGraphBlocks anySatisfy: [ :loopBlock | b id = loopBlock id ] ]
]

{ #category : 'accessing' }
DRLoop >> bodyBlocks [

	^ self blocks select: [ :b | b ~= self headerBlock ]
]

{ #category : 'accessing' }
DRLoop >> bodyHyperblock [

	^ DRHyperBlock blocks: self bodyBlocks.
]

{ #category : 'accessing' }
DRLoop >> bodyInstructions [

	^ self bodyBlocks flatCollect: [ :block | block instructions ]
]

{ #category : 'accessing' }
DRLoop >> condition [

	self flag: #TODO. "See proper way to get condition for all cases"
	^ self loopConditional condition
]

{ #category : 'querying' }
DRLoop >> containsStraightLineCode [

	^ self bodyBlocks size = 1
]

{ #category : 'accessing' }
DRLoop >> controlFlowGraph [

	^ self backJump basicBlock controlFlowGraph
]

{ #category : 'cfg manipulation' }
DRLoop >> createLoopPreheaderBlock [

	| loopPredecessor preheaderBlock |
	loopPredecessor := self preheaderBlock.
	preheaderBlock := loopPredecessor newEmptySuccessors anyOne.
	^ preheaderBlock
]

{ #category : 'cfg manipulation' }
DRLoop >> createLoopPreheaderBlockAndWrapWithIf [
	"The loop pre-header will contain the loop-invariant instructions that are moved out of the loop.
	The if is necessary to preserve semantics in case the loop body is never executed"

	| preheaderBlock loopPredecessor conditionJump headerBlock loopExitBlock preheaderConditionBlock loopConditionJump allDominatedBlocks |

	self headerBlock controlFlowGraph invalidateDominatorTree.
	allDominatedBlocks := self headerBlock dominatedBlocks.


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

	headerBlock controlFlowGraph invalidateDominatorTree.

	headerBlock
		mergeInstructionsWithDuplicatedBlock: preheaderConditionBlock
		withSuccessors: { headerBlock }
		usingUsers: [ :i |
			i nonLocalUsers select: [ :u |
				u isDominatedBy: loopExitBlock firstInstruction ] ]
		dominatedBlocks: allDominatedBlocks.
	^ preheaderBlock
]

{ #category : 'accessing' }
DRLoop >> exitBlock [
	^ self headerBlock successors detect: [ :s | (self bodyBlocks includes: s) not ]
]

{ #category : 'cfg manipulation' }
DRLoop >> fixLoopPhiFunctions: blockCopy [

	self loopPhiFunctions do: [ :phi |
		| phiDependencies loopOutput newBlockInputs newBlockInput newBlockOutput operandToReplace |
		newBlockInputs := blockCopy instructions select: [ :i |
			                  i hasDependencyRecursively: phi ].
		newBlockInputs size = 1 ifFalse: [ "There may be more than one new block input
			(if there are several usages of the same phi functions)"
			1 halt ].

		newBlockInput := newBlockInputs first.
		phiDependencies := self bodyInstructions select: [ :i |
			                   i hasDependency: phi ].
		loopOutput := phiDependencies first.
		newBlockOutput := phiDependencies at: 2 ifAbsent: [ loopOutput ].
		newBlockInput clearUsers.
		operandToReplace := newBlockInput operands detect: [ :o |
			                    o hasDependencyRecursively: phi ].

		newBlockInput replaceDependency: operandToReplace by: loopOutput.
		phi replaceDependency: loopOutput by: newBlockOutput ]
]

{ #category : 'accessing' }
DRLoop >> headerBlock [

	"A loop's header block is a basic block that contains the loop's condition"

	^ self latchBlock successor.
]

{ #category : 'accessing' }
DRLoop >> inductionInitialValues [

	^ self loopPhiFunctions flatCollect: [ :phi |
		  phi valuesForPredecessor: self preheaderBlock ]
]

{ #category : 'accessing' }
DRLoop >> inductionVariable [

	self flag: #TODO. "Handle transitive dependencies"
	^ self loopPhiFunctions
		  detect: [ :phi | self loopConditional hasDependency: phi ]
		  ifNone: [ nil ]
]

{ #category : 'accessing' }
DRLoop >> instructions [

	^ self blocks flatCollect: [ :b | b instructions ]
]

{ #category : 'accessing' }
DRLoop >> isCountingLoop [

	^ self stride ~= 0
]

{ #category : 'accessing' }
DRLoop >> latchBlock [

	"A loop's latch block is a basic block that contains a back jump to the header of the loop"

	^ self backJump basicBlock
]

{ #category : 'accessing' }
DRLoop >> loopConditional [

	"Answer the conditional branch that governs this loop.
	That is the conditional branch that says if
	 - we enter the loop (for another iteration), or
	 - we exit the loop and continue with the rest of the code"
	
	^ self headerBlock endInstruction
]

{ #category : 'accessing' }
DRLoop >> loopGraph [

	^ self controlFlowGraph
		  blocksBetween: self headerBlock
		  and: self latchBlock
]

{ #category : 'cfg manipulation' }
DRLoop >> loopPhiFunctions [

	| loopBody |
	loopBody := self bodyHyperblock.
	^ self headerBlock phiFunctions select: [ :phi | 
		  loopBody instructions anySatisfy: [ :result | 
			  phi operands includes: result ] ]
]

{ #category : 'accessing' }
DRLoop >> preheaderBlock [

	"A loop's pre-header block is the basic block from which the loop is entered.
	There's always a single one if a pre-header block was created using #createLoopPreheaderBlocks,
	but if it was not, this method may fail."

	^ (self headerBlock predecessors asSet copyWithout: self latchBlock) unique
]

{ #category : 'accessing' }
DRLoop >> stride [

	"Returns the value with which the loop's induction variable is incremented on each loop iteration, or 0 if the value is unknown or not constant"

	| inductionVariableModifierInstructions inductionVariableModifierInstruction |
	self flag: #TODO.
	"- Support induction variable being modified through more than one instruction
	 - Support non-incrementing modifications"
	inductionVariableModifierInstructions := self inductionVariable users 
		intersection: self inductionVariable dependencies.
	inductionVariableModifierInstructions size = 1 ifFalse: [ ^ 0 ].
	inductionVariableModifierInstruction := inductionVariableModifierInstructions anyOne.
	^ inductionVariableModifierInstruction isAdd
		  ifTrue: [ 
			  | stride |
			  stride := (inductionVariableModifierInstruction operands 
				             copyWithout: self inductionVariable) first.
			  stride isConstant
				  ifTrue: [ stride value ]
				  ifFalse: [ 0 ] ]
		  ifFalse: [ 0 ]
]

{ #category : 'operations' }
DRLoop >> unroll: unrollingFactor [

	| isIncrementingLoop rolledLoop rolledLoopCopy loopBody bodyCopy copiedLoop loopCondition nonInductionOperand decrementInstruction |
	loopCondition := self condition.
	isIncrementingLoop := loopCondition isLessThanComparison or: [
		                      loopCondition isLessOrEqualThanComparison ].
	self isCountingLoop & isIncrementingLoop ifFalse: [ ^ self ].

	"Add loop copy for remaining iterations at the end (that cannot be processed by the unrolled loop)"
	rolledLoop := DRHyperBlock blocks: self blocks.
	rolledLoopCopy := DRHyperBlock blocks: rolledLoop duplicate values.
	rolledLoopCopy addBetween: self headerBlock and: self exitBlock.
	rolledLoopCopy removePredecessor: self preheaderBlock.

	copiedLoop := (rolledLoopCopy instructions detect: [ :i |
		               i isBackJump ]) loop.
	self controlFlowGraph invalidateDominatorTree.
	self loopPhiFunctions
		with: copiedLoop loopPhiFunctions
		do: [ :unrolledLoopPhi :rolledLoopPhi |
			unrolledLoopPhi replaceDominatedUsesBy: rolledLoopPhi.
			rolledLoopPhi
				replaceOperandAtPredecessor: unrolledLoopPhi basicBlock
				by: unrolledLoopPhi ].

	"Modify unrolled loop's condition to prevent additional iterations"
	nonInductionOperand := self loopConditional operands detect: [ :o |
		                       o ~= self inductionVariable ].
	decrementInstruction := self headerBlock instructionFactory
		                        subtract: unrollingFactor * self stride - 1
		                        from: nonInductionOperand.
	self headerBlock add: decrementInstruction before: self loopConditional.
	self loopConditional
		replaceOperand: nonInductionOperand
		by: decrementInstruction.

	"Unroll loop body"
	loopBody := self bodyHyperblock.
	bodyCopy := loopBody.
	1 to: unrollingFactor - 1 do: [ :_ |
		| oldLastBlock |
		bodyCopy := DRHyperBlock blocks: bodyCopy duplicate values.
		oldLastBlock := self bodyHyperblock exitBlock.

		bodyCopy removePredecessors.
		bodyCopy addBetween: oldLastBlock and: self headerBlock.
		self backJump: bodyCopy exitBlock endInstruction ].
	self fixLoopPhiFunctions: bodyCopy
]
