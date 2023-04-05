Class {
	#name : #DRCleanControlFlow,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #testing }
DRCleanControlFlow >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph allJumps anySatisfy: [ :j | 
		  j isConditionalBranch or: [ j target predecessors size = 1 ] ]
]

{ #category : #combining }
DRCleanControlFlow >> combineBlocks: aDRBasicBlock and: aDRBasicBlock2 fromCfg: aCFG [
	
	"Assume that
	 - the first block contains a non conditional jump to the second block
	 - the second block has as single predecessor the first block"
	
	| combinedBlock endInstruction |
	combinedBlock := aCFG newBasicBlock.
	aDRBasicBlock predecessors copy do: [ :predecessor |
		combinedBlock addPredecessor: predecessor.
		predecessor endInstruction replaceTarget: aDRBasicBlock by: combinedBlock ].

	aDRBasicBlock instructions allButLastDo: [ :e |
		"Ignore the jump"
		aDRBasicBlock removeInstruction: e.
		combinedBlock addInstruction: e.
	].
	aDRBasicBlock2 instructions allButLastDo: [ :e |
		"Ignore the jump"
		aDRBasicBlock2 removeInstruction: e.
		combinedBlock addInstruction: e.
	].
	
	aDRBasicBlock2 successors do: [ :e |
		e replacePredecessor: aDRBasicBlock2 by: combinedBlock ].

	endInstruction := aDRBasicBlock2 endInstruction.
	aDRBasicBlock2 basicEndInstruction: DRNoop new.
	combinedBlock endInstruction: endInstruction.

	aCFG removeBasicBlock: aDRBasicBlock.
	aCFG removeBasicBlock: aDRBasicBlock2.
]

{ #category : #executing }
DRCleanControlFlow >> doApply: aCFG [

	| didChange counter |
	didChange := true.
	counter := 0.
	[ didChange ] whileTrue: [
		counter := counter + 1.
		didChange := self step: aCFG.
	]
]

{ #category : #folding }
DRCleanControlFlow >> foldRedundantBranch: aDRConditionalJump [ 
	
	aDRConditionalJump replaceBy: (DRJump new target: aDRConditionalJump trueBranch).
	aDRConditionalJump trueBranch removePredecessor: aDRConditionalJump basicBlock.
]

{ #category : #hoisting }
DRCleanControlFlow >> hoistBranchInto: aDRBasicBlock [ 

	"Assume the basic branch has a single non conditional jump.
	And that jump targets an empty block with a conditional branch"
	
	| skippedBlock |
	skippedBlock := aDRBasicBlock successor.
	aDRBasicBlock endInstruction: skippedBlock endInstruction copy.

	aDRBasicBlock successors do: [ :e |
		e addPredecessor: aDRBasicBlock ]
]

{ #category : #'empty blocks' }
DRCleanControlFlow >> removeEmptyBlock: aDRBasicBlock fromCfg: aCFG [
	
	"Assume this is an empty block with a single non-conditional jump.
	Rewire all predecessors to point to my single successor.
	Require my single successor to have as predecessors all my predecessors"
	
	"Implement this by first duplicating all the predecessor links"
	
	aDRBasicBlock predecessors copy do: [ :predecessor |
		aDRBasicBlock basicRemovePredecessor: predecessor.
		"Duplicate should keep the phi functions valid"
		aDRBasicBlock successor duplicatePredecessor: aDRBasicBlock from: predecessor.
		predecessor endInstruction replaceTarget: aDRBasicBlock by: aDRBasicBlock successor
	].

	"Remove the predecessor from our successor at the end"
	aDRBasicBlock successor removePredecessor: aDRBasicBlock.
	aDRBasicBlock basicEndInstructionRemovingUsers: DRNoop new.
	aCFG removeBasicBlock: aDRBasicBlock
]

{ #category : #combining }
DRCleanControlFlow >> shouldCombineBlock: aDRBasicBlock [
	
	"Assume that
	 - the first block contains a non conditional jump to the second block
	 - the second block has as single predecessor the first block"
	aDRBasicBlock endInstruction isNonConditionalJump ifFalse: [ ^ false ].
	
	"Cannot combine blocks if the second block has phi functions.
	This is because this may break phi function invariants such as:
		- the operand of the phi function may not be in a predecessor but the block itself
		- merging could increment the number of predecessors while keeping phi functions with the same arity"
	^ aDRBasicBlock successor predecessors size = 1
		and: [ 
			aDRBasicBlock successor isExitBlock not
				and: [ aDRBasicBlock successor instructions noneSatisfy: [ :i | i isPhiFunction ] ] ]
]

{ #category : #folding }
DRCleanControlFlow >> shouldFoldRedundantBranch: aDRConditionalJump [ 

	"If they are not the same branch, we cannot fold"	
	| target |
	aDRConditionalJump trueBranch = aDRConditionalJump falseBranch ifFalse: [ ^ false ].
	
	"Otherwise, we fold only if the target has phi functions with values that can be folded"
	target := aDRConditionalJump trueBranch.
	^ target phiFunctions allSatisfy: [ :phi |
		(phi valuesForPredecessor: aDRConditionalJump basicBlock)
			asSet size = 1 ].
]

{ #category : #folding }
DRCleanControlFlow >> shouldFoldRedundantBranchIn: aBlock [ 
	
	^ aBlock endInstruction isConditionalBranch and: [ 
		self shouldFoldRedundantBranch: aBlock endInstruction ]
]

{ #category : #hoisting }
DRCleanControlFlow >> shouldHoistBranchInto: aDRBasicBlock [ 

	"
	- the basic branch has a single non conditional jump.
	- and that jump targets an empty branch with a conditional branch"
	^ aDRBasicBlock endInstruction isNonConditionalJump and: [ | targetInstructions |
		targetInstructions := aDRBasicBlock endInstruction target instructions.
		targetInstructions size = 1 and: [ targetInstructions first isConditionalBranch ] ]
]

{ #category : #'empty blocks' }
DRCleanControlFlow >> shouldRemoveEmptyBlock: aDRBasicBlock [ 
	
	aDRBasicBlock isExitBlock ifTrue: [ ^ false ].
	
	aDRBasicBlock instructions size > 1
		ifTrue: [ ^ false ].
	aDRBasicBlock endInstruction isNonConditionalJump
		ifFalse: [ ^ false ].
		
	"If removing this block would make my predecessor point twice to the same successor, do not do it"
	((aDRBasicBlock predecessors anySatisfy: [ :e |
		e successors includes: aDRBasicBlock successor ])
			and: [ aDRBasicBlock successor hasPhiFunctions ])
				ifTrue: [ ^ false ].
	
	"Avoid complex phi function rewriting"
	(aDRBasicBlock predecessors size > 1 and: [ aDRBasicBlock successor hasPhiFunctions ])
		ifTrue: [ ^ false ].
	^ true
]

{ #category : #executing }
DRCleanControlFlow >> step: cfg [

	| didChange |
	didChange := false.
	cfg postOrderBlocks allButLast do: [ :e |
		didChange := didChange or: [ self stepBlock: e fromCfg: cfg ] ].
	^ didChange
]

{ #category : #executing }
DRCleanControlFlow >> stepBlock: aDRBasicBlock fromCfg: aCFG [
	
	(self shouldFoldRedundantBranchIn: aDRBasicBlock) ifTrue: [ 
		self foldRedundantBranch: aDRBasicBlock endInstruction.
		^ true ].
	(self shouldRemoveEmptyBlock: aDRBasicBlock) ifTrue: [ 
		self removeEmptyBlock: aDRBasicBlock fromCfg: aCFG.
		^ true ].
	(self shouldCombineBlock: aDRBasicBlock) ifTrue: [ 
		self combineBlocks: aDRBasicBlock and: aDRBasicBlock successor fromCfg: aCFG.
		^ true ].
	(self shouldHoistBranchInto: aDRBasicBlock) ifTrue: [ 
		self hoistBranchInto: aDRBasicBlock.
		^ true ].
	^ false
]
