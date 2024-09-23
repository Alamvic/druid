Class {
	#name : #DRCogitStackCoalescingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #tests }
DRCogitStackCoalescingTest >> setUp [

	optimisation := DRCogitStackCoalescing new
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceOneSpilledAndOneUnspilledPopInsertsUnspill [

	| cfg basicBlock1 basicBlock2 mergeBlock popToUnspill popStackDepth |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		popToUnspill := block pop
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	popStackDepth := popToUnspill stackDepth.

	optimisation applyTo: cfg.

	self assert: basicBlock2 firstInstruction isUnspill.
	self assert: basicBlock2 firstInstruction operand1 value equals: popStackDepth negated - 1
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceOneSpilledAndOneUnspilledPopsIntoSingleUnpilledPop [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	self assert: mergeBlock firstInstruction isPop.
	self assert: mergeBlock firstInstruction isUnspilled
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceOneUnspilledAndOneSpilledPopInsertsUnspill [

	| cfg basicBlock1 basicBlock2 mergeBlock popToUnspill popStackDepth |
	cfg := DRControlFlowGraph new.
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		popToUnspill := block pop
 	].
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	popStackDepth := popToUnspill stackDepth.
	
	optimisation applyTo: cfg.

	self assert: basicBlock2 firstInstruction isUnspill.
	self assert: basicBlock2 firstInstruction operand1 value equals: popStackDepth negated - 1
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceOneUnspilledAndOnespilledPopsIntoSingleUnpilledPop [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	self assert: mergeBlock firstInstruction isPop.
	self assert: mergeBlock firstInstruction isUnspilled
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceTwoSpilledPopsDoesNotReplaceMovedInstructions [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	"Blocks only contain the jumps"
	self assert: basicBlock1 instructions size equals: 1.
	self assert: basicBlock2 instructions size equals: 1.
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceTwoSpilledPopsIntoSingleSpilledPop [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block pop
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	self assert: mergeBlock firstInstruction isPop.
	self deny: mergeBlock firstInstruction isUnspilled
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceTwoUnspilledPopsDoesNotReplaceMovedInstructions [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	"Blocks only contain the jumps"
	self assert: basicBlock1 instructions size equals: 1.
	self assert: basicBlock2 instructions size equals: 1.
]

{ #category : #tests }
DRCogitStackCoalescingTest >> testCoalesceTwoUnspilledPopsIntoSingleUnspilledPop [

	| cfg basicBlock1 basicBlock2 mergeBlock |
	cfg := DRControlFlowGraph new.
	basicBlock1 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	basicBlock2 := cfg newBasicBlockWith: [ :block |
		block popUnspilled
 	].
	cfg initialBasicBlock jumpIf: true to: basicBlock1 ifFalseTo: basicBlock2.
	mergeBlock := cfg newBasicBlock.
	basicBlock1 jumpTo: mergeBlock.
	basicBlock2 jumpTo: mergeBlock.
	
	optimisation applyTo: cfg.

	self assert: mergeBlock firstInstruction isPop.
	self assert: mergeBlock firstInstruction isUnspilled
]
