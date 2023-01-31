Class {
	#name : #DRRedundantCodeEliminationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRRedundantCodeEliminationTest >> testRedundantCopyIsEliminated [

	| cfg b1 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | 
		      block addInstruction: (DRCopy
				       operands: { (DRPhysicalGeneralPurposeRegister name: 'argument') }
				       result: (DRPhysicalGeneralPurposeRegister name: 'argument')) 
	].

	DRRedundantCopyElimination new applyTo: cfg.

	self assert: b1 instructions size equals: 1. "Noop"
]

{ #category : #tests }
DRRedundantCodeEliminationTest >> testValueCopyIsNotEliminated [

	| cfg b1 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | 
		 block copy: 1 asDRValue
	].

	DRRedundantCopyElimination new applyTo: cfg.

	self assert: b1 instructions size equals: 2. "Copy + Noop"
]
