Class {
	#name : #DRControlFlowManipulationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRControlFlowManipulationTest >> testRemoveInstructionWithDuplicatedOperandRemovesBackpointer [

	| cfg r0 r1 |
	cfg := DRControlFlowGraph new.
	
	cfg newBasicBlockWith: [ :b |
		"R0 := 1"
		r0 := b copy: 1.
		
		"R1 := phi(R0, R0)"
		r1 := b phiWith: r0 with: r0.
	].
	
	r1 removeFromCFG.
	
	self assert: r0 users isEmpty
]

{ #category : #tests }
DRControlFlowManipulationTest >> testRemovePredecessorRemovesPhiArgument [

	| cfg comingFrom1 pred1 comingFrom2 pred2 copy1 b1 phiFunction |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlockWith: [ :b | comingFrom1 := b copy: 1 ].
	pred2 := cfg newBasicBlockWith: [ :b | comingFrom2 := b copy: 2 ].
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := Phi(comingFrom1, comingFrom2)"
		copy1 := b phiWith: comingFrom1 with: comingFrom2.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.
	
	b1 removePredecessor: pred1.
	
	phiFunction := b1 instructions first.
	self assertCollection: phiFunction operands hasSameElements: {comingFrom2}.
	self assert: (comingFrom2 users includes: phiFunction).
	self deny: (comingFrom1 users includes: phiFunction).
]

{ #category : #tests }
DRControlFlowManipulationTest >> testRemovePredecessorWithRedundantPhiKeepsPhiOperandUser [

	| cfg comingFromTop pred1 pred2 copy1 b1 phiFunction init |
	cfg := DRControlFlowGraph new.
	init := cfg newBasicBlockWith: [ :b | 
		comingFromTop := b copy: 1 ].
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	init endInstruction: (cfg instructionFactory jumpIf: true).
	init endInstruction trueBranch: pred1.
	init endInstruction falseBranch: pred2.
	
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := Phi(comingFromTop, comingFromTop)"
		copy1 := b phiWith: comingFromTop with: comingFromTop.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.
	
	b1 removePredecessor: pred1.
	
	phiFunction := b1 instructions first.
	self assertCollection: phiFunction operands hasSameElements: {comingFromTop}.
	self assert: (comingFromTop users includes: phiFunction)
]
