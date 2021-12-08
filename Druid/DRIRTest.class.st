Class {
	#name : #DRIRTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRIRTest >> testRemoveInstructionWithDuplicatedOperandRemovesBackpointer [

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
