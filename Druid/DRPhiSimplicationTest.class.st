Class {
	#name : #DRPhiSimplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRPhiSimplicationTest >> testNotSimplificateComplexPhi [

	| cfg b1 t1 t2 t3 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | 
		      t1 := block copy: 1 asDRValue.
		      t2 := block copy: 2 asDRValue.
		      t3 := block phiWith: t1 with: t2].

	DRPhiSimplication new applyTo: cfg.
	
	self assert: (b1 instructions at: 3) equals: t3
]

{ #category : #tests }
DRPhiSimplicationTest >> testSimplificateSimplePhi [

	| cfg b1 t1 t2 newT2 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlockWith: [ :block | 
		      t1 := block copy: 1 asDRValue.
		      t2 := block phiWith: t1 ].

	DRPhiSimplication new applyTo: cfg.
	
	newT2 := b1 instructions at: 2.
	self assert: newT2 isCopy.
	self assert: newT2 operand1 equals: t1
]
