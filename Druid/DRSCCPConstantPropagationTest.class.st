Class {
	#name : #DRSCCPConstantPropagationTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateAndFoldInComplexInstructionLeft [

	| optimisation bb cfg copy1 operandList|
	optimisation := DRSCCPConstantPropagation new.
	bb := DRBasicBlock new.
	 
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	
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
DRSCCPConstantPropagationTest >> testPropagateAndFoldInComplexInstructionRight [

	| optimisation bb cfg copy1 operandList|
	optimisation := DRSCCPConstantPropagation new.
	bb := DRBasicBlock new.
	
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.
	
	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := 2 + T1"
	
	operandList := OrderedCollection new. 
	operandList add: (DRConstantValue value: 2). 
	operandList add: copy1. 
	
	bb addInstruction: (DRAdd operands: operandList result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 2).
	self assert: cfg instructions last operand2 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateBetweenBasicBlocks [

	| optimisation bb cfg copy1 bb2 |
	optimisation := DRSCCPConstantPropagation new.
	bb := DRBasicBlock new.
	bb2 := DRBasicBlock new.
	bb jumpTo: bb2.
	
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.

	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	bb2 addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateDifferentValuesInPhi [

	| optimisation bb cfg copy1 leftBlock rightBlock copy2 jump exitBasicBlock |
	optimisation := DRSCCPConstantPropagation new.
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
	
	exitBasicBlock := DRBasicBlock new.
	leftBlock jumpTo: exitBasicBlock.
	rightBlock jumpTo: exitBasicBlock.
	
	"T3 := ø(T1, T2)"
	exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	"The T1 access got replaced by the contant 1"
	self assert: cfg instructions last equals: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateInSingleBasicBlock [

	| optimisation bb cfg copy1 |
	optimisation := DRSCCPConstantPropagation new.
	bb := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb.

	"T1 := 1"
	bb addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	"T2 := T1"
	bb addInstruction: (DRCopy operands: { copy1 } result:  (DRTemporaryRegister id: 2)).
	optimisation applyTo: cfg.

	"The T1 access got replaced by the constant 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagatePhiOnDifferentValues [

	| optimisation bb1 bb2 bb3 cfg copy1 leftBlock rightBlock copy2 jump phiInstruction copyInstruction |
	optimisation := DRSCCPConstantPropagation new.
	bb1 := DRBasicBlock new.
	bb2 := DRBasicBlock new.
	bb3 := DRBasicBlock new.
		
	cfg := DRControlFlowGraph new.
	cfg initialBasicBlock: bb1.
	
	
	leftBlock := DRBasicBlock new.
	"T1 := 1"
	leftBlock addInstruction: (copy1 := DRCopy operands: { DRConstantValue value: 1 } result:  (DRTemporaryRegister id: 1)).
	
	rightBlock := DRBasicBlock new.
	"T2 := 2"
	rightBlock addInstruction: (copy2 := DRCopy operands: { DRConstantValue value: 2 } result:  (DRTemporaryRegister id: 2)).
	
	cfg initialBasicBlock endInstruction: (jump := DRConditionalJump operands: { DRConstantValue value: true }).
	jump trueBranch: leftBlock.
	jump falseBranch: rightBlock.
	
	leftBlock jumpTo: bb2.
	rightBlock jumpTo: bb2.
	
	bb2 jumpTo: bb3. 
	
	"T3 := ø(T1, T2)"
	phiInstruction := ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
	bb2 addInstruction: phiInstruction. 
	
	copyInstruction := DRCopy operands: { phiInstruction } result: {DRTemporaryRegister id: 4}. 
	bb3 addInstruction: copyInstruction . 
	

	optimisation applyTo: cfg.
	
	"The T1 access got replaced by the contant 1"
	self assert: cfg instructions last equals: copyInstruction. 
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagatePhiOnSameValue [

	| optimisation bb cfg copy1 leftBlock rightBlock copy2 jump phi exitBasicBlock |
	optimisation := DRSCCPConstantPropagation new.
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
	
	exitBasicBlock := DRBasicBlock new.
	leftBlock jumpTo: exitBasicBlock.
	rightBlock jumpTo: exitBasicBlock.
	
	"T3 := ø(T1, T2)"
	phi := ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).
	exitBasicBlock addInstruction: phi. 
	"T4 := T3"
	exitBasicBlock addInstruction: (DRCopy operands: {phi} result: (DRTemporaryRegister id: 4)). 


	optimisation applyTo: cfg.
	
	"T4 := T3 should become T4 := 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]

{ #category : #tests }
DRSCCPConstantPropagationTest >> testPropagateSameValuesInPhi [

	| optimisation bb cfg copy1 leftBlock rightBlock copy2 jump exitBasicBlock |
	optimisation := DRSCCPConstantPropagation new.
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
	
	exitBasicBlock := DRBasicBlock new.
	leftBlock jumpTo: exitBasicBlock.
	rightBlock jumpTo: exitBasicBlock.
	
	"T3 := ø(T1, T2)"
	exitBasicBlock addInstruction: ((DRPhiFunction joinedVariables: { copy1. copy2 })
		result: (DRTemporaryRegister id: 3);
		yourself).

	optimisation applyTo: cfg.
	
	"T3 := ø(T1, T2) should become T3 := 1"
	self assert: cfg instructions last operand1 equals: (DRConstantValue value: 1)
]