Class {
	#name : #DRPhiSimplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRPhiSimplicationTest >> testNotSimplificateDualPhi [

	| cfg t1 t2 t3 b2 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block | 
		      t1 := block copy: 1 asDRValue.
		      t2 := block copy: 2 asDRValue ].
	
	b2 := cfg newBasicBlockWith: [ :block | 
		      t3 := block phiWith: t1 with: t2 ].

	DRPhiSimplication new applyTo: cfg.

	self assert: b2 firstInstruction equals: t3
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplificateEmptyPhi [

	| cfg b |
	cfg := DRControlFlowGraph new.

	b := cfg newBasicBlockWith: [ :block | 
		     block addInstruction:
			     (block instructionFactory phiWithVariables: {  }) ].

	DRPhiSimplication new applyTo: cfg.

	self assert: b firstInstruction isNoop
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplificateRedundantPhi [

	| cfg b t1 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block | 
		      t1 := block copy: 1 asDRValue ].

	b := cfg newBasicBlockWith: [ :block | block phiWith: t1 with: t1 ].

	DRPhiSimplication new applyTo: cfg.

	self assert: b firstInstruction isCopy.
	self assert: b firstInstruction operand1 equals: t1
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplificateSimplePhi [

	| cfg b t1 |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :block | t1 := block copy: 1 asDRValue ].

	b := cfg newBasicBlockWith: [ :block | block phiWith: t1 ].


	DRPhiSimplication new applyTo: cfg.

	self assert: b firstInstruction isCopy.
	self assert: b firstInstruction operand1 equals: t1.
	self assert: t1 users size equals: 1.
	self assert: t1 users anyOne equals: b firstInstruction
]
