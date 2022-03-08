Class {
	#name : #DRCopyPropagationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsNotPropagatedBetweenBranches [

	| cfg instruction |
	cfg := self generateDruidIRFor: #branchingWithAssigments.

	DRCopyPropagation new applyTo: cfg.

	instruction := cfg instructionByResultId: 2.
	self assert: instruction users size equals: 1.
	self assert: instruction users anyOne isCopy
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsNotPropagatedFromConstantToPhiFunctions [

	| cfg t1 t2 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block |
		t1 := block copy: 1 asDRValue.
		t2 := block phiWith: t1.
	].

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t2 operands first equals: t1
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsNotPropagatedFromObjectReferenceToPhiFunctions [

	| cfg t1 t2 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block |
		t1 := block copy: DRTrueObject new.
		t2 := block phiWith: t1.
	].

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t2 operands first equals: t1
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsPropagatedInBlockSequence [

	| cfg t1 t2 b1 b2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block |
		t1 := block copy: (DRPhysicalRegister name: 'argument').
	].

	b1 jumpTo: (b2 := cfg newBasicBlockWith: [ :block |
		t2 := block copy: t1.
	]).

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t2 operands first equals: t1 operands first
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsPropagatedInSingleBlock [

	| cfg t1 t2 t3 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block |
		t1 := block copy: (DRPhysicalRegister name: 'argument').
		t2 := block copy: t1.
		t3 := block copy: t2.
	].

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t3 operand1 equals: t1 operand1
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsPropagatedInSingleBlockToManyInstructions [

	| cfg t1 t2 t3 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block |
		t1 := block copy: (DRPhysicalRegister name: 'argument').
		t2 := block copy: t1.
		t3 := block copy: t1.
	].

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t3 operands first equals: t2 operands first
]

{ #category : #tests }
DRCopyPropagationTest >> testCopyIsPropagatedInSingleBlockWithSequencesOfUsages [

	| cfg t1 t2 t3 t4 t5 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block |
		t1 := block copy: (DRPhysicalRegister name: 'argument').
		t2 := block copy: t1.
		t3 := block copy: t2.
		t4 := block copy: t3.
		t5 := block copy: t4.
	].

	DRCopyPropagation new applyTo: cfg.
	
	self assert: t5 operands first equals: t1 operands first
]
