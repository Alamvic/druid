Class {
	#name : #DRCleanControlFlow,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #executing }
DRCleanControlFlow >> applyTo: aCFG [

	| didChange |
	didChange := true.
	[ didChange ] whileTrue: [
		didChange := self step: aCFG
	]
]

{ #category : #combining }
DRCleanControlFlow >> combineBlocks: aDRBasicBlock and: aDRBasicBlock2 [ 
	
	"Assume that
	 - the first block contains a non conditional jump to the second block
	 - the second block has as single predecessor the first block"
	
	| combinedBlock |
	combinedBlock := DRBasicBlock new.
	aDRBasicBlock instructions allButLastDo: [ :e |
		"Ignore the jump"
		combinedBlock addInstruction: e.
	].
	aDRBasicBlock2 instructions allButLastDo: [ :e |
		"Ignore the jump"
		combinedBlock addInstruction: e.
	].
	combinedBlock endInstruction: aDRBasicBlock2 endInstruction.
	
	aDRBasicBlock predecessors do: [ :e |
		combinedBlock addPredecessor: e.
		e endInstruction replaceTarget: aDRBasicBlock by: combinedBlock ].
	aDRBasicBlock2 successors do: [ :e |
		e removePredecessor: aDRBasicBlock2.
		e addPredecessor: combinedBlock ]
]

{ #category : #folding }
DRCleanControlFlow >> foldRedundantBranch: aDRConditionalJump [ 
	
	aDRConditionalJump replaceBy: (DRJump new target: aDRConditionalJump trueBranch)
		
]

{ #category : #hoisting }
DRCleanControlFlow >> hoistBranchInto: aDRBasicBlock [ 

	"Assume the basic branch has a single non conditional jump.
	And that jump targets an empty branch with a conditional branch"
	
	| skippedBlock |
	skippedBlock := aDRBasicBlock successor.
	aDRBasicBlock endInstruction: skippedBlock endInstruction.
	aDRBasicBlock successors do: [ :e |
		e removePredecessor: skippedBlock.
		e addPredecessor: aDRBasicBlock ]
]

{ #category : #'empty blocks' }
DRCleanControlFlow >> removeEmptyBlock: aDRBasicBlock [ 
	
	"Assume this is an empty block with a single non-conditional jump.
	Rewire all predecessors to point to my single successor.
	Require my single successor to have as predecessors all my predecessors"
	
	aDRBasicBlock successor removePredecessor: aDRBasicBlock.
	aDRBasicBlock predecessors do: [ :e |
		e endInstruction replaceTarget: aDRBasicBlock by: aDRBasicBlock successor.
		aDRBasicBlock successor addPredecessor: e.
	]
]

{ #category : #combining }
DRCleanControlFlow >> shouldCombineBlock: aDRBasicBlock [
	
	"Assume that
	 - the first block contains a non conditional jump to the second block
	 - the second block has as single predecessor the first block"
	
	^ aDRBasicBlock endInstruction isNonConditionalJump and: [ 
		aDRBasicBlock successor predecessors size = 1 ]
]

{ #category : #folding }
DRCleanControlFlow >> shouldFoldRedundantBranch: aDRConditionalJump [ 
	
	^ aDRConditionalJump trueBranch = aDRConditionalJump falseBranch
]

{ #category : #folding }
DRCleanControlFlow >> shouldFoldRedundantBranchIn: aBlock [ 
	
	^ aBlock endInstruction isConditionalJump and: [ 
		self shouldFoldRedundantBranch: aBlock endInstruction ]
]

{ #category : #hoisting }
DRCleanControlFlow >> shouldHoistBranchInto: aDRBasicBlock [ 

	"
	- the basic branch has a single non conditional jump.
	- and that jump targets an empty branch with a conditional branch"
	^ aDRBasicBlock endInstruction isNonConditionalJump and: [ | targetInstructions |
		targetInstructions := aDRBasicBlock endInstruction target instructions.
		targetInstructions size = 1 and: [ targetInstructions first isConditionalJump ] ]
]

{ #category : #'empty blocks' }
DRCleanControlFlow >> shouldRemoveEmptyBlock: aDRBasicBlock [ 
	
	^ aDRBasicBlock instructions size = 1
		and: [ aDRBasicBlock endInstruction isNonConditionalJump ]
]

{ #category : #executing }
DRCleanControlFlow >> step: cfg [

	| didChange |
	didChange := false.
	cfg postOrderBlocks allButLast do: [ :e |
		didChange := didChange or: [self stepBlock: e]
	].
	^ didChange
]

{ #category : #executing }
DRCleanControlFlow >> stepBlock: aDRBasicBlock [

	(self shouldFoldRedundantBranchIn: aDRBasicBlock) ifTrue: [ 
		self foldRedundantBranch: aDRBasicBlock endInstruction.
		^ true ].
	(self shouldRemoveEmptyBlock: aDRBasicBlock) ifTrue: [ 
		self removeEmptyBlock: aDRBasicBlock.
		^ true ].
	(self shouldCombineBlock: aDRBasicBlock) ifTrue: [ 
		self combineBlocks: aDRBasicBlock and: aDRBasicBlock successor.
		^ true ].
	(self shouldHoistBranchInto: aDRBasicBlock) ifTrue: [ 
		self hoistBranchInto: aDRBasicBlock.
		^ true ].
	^ false
]
