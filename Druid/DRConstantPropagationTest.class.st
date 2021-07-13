Class {
	#name : #DRConstantPropagationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRConstantPropagationTest >> testPropagateBetweenBasicBlocks [

	| optimisation bb cfg copy1 bb2 |
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
	bb2 := DRBasicBlock new.
	bb jumpTo: bb2.
	
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	cfg exitBasicBlock: bb2.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	bb2 addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRConstantPropagationTest >> testPropagateDifferentValuesInPhi [

	| optimisation bb cfg copy1 leftBlock rightBlock copy2 jump |
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	
	
	leftBlock := DRBasicBlock new.
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	rightBlock := DRBasicBlock new.
	"T2 := 2"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)).
	
	cfg initialBasicBlock endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	cfg exitBasicBlock: DRBasicBlock new.
	leftBlock jumpTo: cfg exitBasicBlock.
	rightBlock jumpTo: cfg exitBasicBlock.
	
	"T3 := ø(T1, T2)"
	cfg exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	"The T1 access got replaced by the contant 1"
	self assert: cfg instructions last equals: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
]

{ #category : #tests }
DRConstantPropagationTest >> testPropagateInComplexInstructionLeft [

	| optimisation bb cfg copy1 operandList|
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
	 
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	cfg exitBasicBlock: bb.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1 + 2"
	
	operandList := OrderedCollection new.
	operandList add: copy1. 
	operandList add: (DRConstantValue value: 1). 
	
	bb addInstruction: (DRAdd operands: operandList result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand2 equals: (DRConstantValue value: 1).
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 2)
	
]

{ #category : #tests }
DRConstantPropagationTest >> testPropagateInComplexInstructionRight [

	| optimisation bb cfg copy1 operandList|
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
	
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	cfg exitBasicBlock: bb.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := 2 + T1"
	
	operandList := OrderedCollection new. 
	operandList add: (DRConstantValue value: 2). 
	operandList add: copy1. 
	
	bb addInstruction: (DRAdd operands: operandList result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1).
	self assert: cfg instructions last operand2 equals: (DRConstantValue value: 2)
]

{ #category : #tests }
DRConstantPropagationTest >> testPropagateInSingleBasicBlock [

	| optimisation bb cfg copy1 |
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	cfg exitBasicBlock: bb.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	bb addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRConstantPropagationTest >> testPropagateSameValuesInPhi [

	| optimisation bb cfg copy1 leftBlock rightBlock copy2 jump |
	optimisation := DRConstantPropagation new.
	bb := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	
	
	leftBlock := DRBasicBlock new.
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	rightBlock := DRBasicBlock new.
	"T2 := 1"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 2)).
	
	cfg initialBasicBlock endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	leftBlock addPredecessor: cfg initialBasicBlock.
	jump trueBranch: leftBlock.
	rightBlock addPredecessor: cfg initialBasicBlock.
	jump falseBranch: rightBlock.
	
	cfg exitBasicBlock: DRBasicBlock new.
	leftBlock jumpTo: cfg exitBasicBlock.
	rightBlock jumpTo: cfg exitBasicBlock.
	
	"T3 := ø(T1, T2)"
	cfg exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	"T3 := ø(T1, T2) should become T3 := 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]
