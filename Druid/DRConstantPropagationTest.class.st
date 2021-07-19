Class {
	#name : #DRConstantPropagationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRConstantPropagationTest >> testPropagateInSingleBasicBlock [

	| optimisation bb cfg copy1 |
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	bb addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	
	optimisation applyTo: cfg.
	
	"The T1 access got replaced by the contant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]
