Class {
	#name : #DRSCCPConstantFoldingTest,
	#superclass : #TestCase,
	#instVars : [
		'n',
		'optimisation',
		'rightAddInstruction',
		'instructionFactory',
		'leftAddInstruction',
		'leftOperand1',
		'leftOperand2',
		'rightOperand1',
		'rightOperand2'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRSCCPConstantFoldingTest >> assertLastInstructionOf: cfg isCopyValue: aValue [

	self assert: cfg instructions allButLast last isCopy.
	self assert: cfg instructions allButLast last operand1 equals: aValue asDRValue.
]

{ #category : #running }
DRSCCPConstantFoldingTest >> setUp [

	super setUp.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRSCCP new.
	
	"Set the operands of the add instructions"
	leftOperand1 := 3.
	leftOperand2 := 4.
	
	rightOperand1 := 5.
	rightOperand2 := 6.
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> setUpCFG: n [

	| cfg b0 b1 b2 b3 |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	
	cfg initialBasicBlock: b0.
	
	n = 1 ifTrue: [ 
		b0 endInstruction: DRNoop new.
		cfg initialBasicBlock: b0. 
		cfg b0: b0. ].
	
	n = 2 ifTrue: [ 
		b1 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		
		cfg b0: b0. 
		cfg b1: b1. ].

	n = 3 ifTrue: [ 
		b2 endInstruction: DRNoop new.
		b0 jumpTo: b1.
		b1 jumpTo: b2. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. ].
	
	n = 4 ifTrue: [ 
		b3 endInstruction: DRNoop new. 
		b0 jumpTo: b1. 
		b1 jumpTo: b2. 
		b2 jumpTo: b3. 
		
		cfg b0: b0.
		cfg b1: b1. 
		cfg b2: b2. 
		cfg b3: b3. ].

	^ cfg. 



















]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> setUpCFGWithConditional: jump [

	| cfg b0 b1 b2 b3 b4|
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := DRBasicBlock id: 0.
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4. 
	
	cfg initialBasicBlock: b0.
	
	b4 endInstruction: DRNoop new. 
	b0 jumpTo: b1.
	
	jump condition isInstruction ifTrue: [
		b1 addInstruction: jump condition.
	].
	b1 endInstruction: jump. 
	jump trueBranch: b2.
	jump falseBranch: b3.
	b2 jumpTo: b4. 
	b3 jumpTo: b4. 
		
	cfg b0: b0.
	cfg b1: b1. 
	cfg b2: b2. 
	cfg b3: b3. 
	cfg b4: b4. 
	cfg conditionnalJump: jump. 

	^ cfg. 



















]

{ #category : #tests }
DRSCCPConstantFoldingTest >> setUpControlFlowGraphWithConditionalJump: aJump [

	| cfg copy3 copy4 copy5 copy6 |
	cfg := self setUpCFGWithConditional: aJump.

	"This block will always be executed"
	cfg b2 addInstruction: (copy3 := instructionFactory copy: leftOperand1).
	cfg b2 addInstruction: (copy4 := instructionFactory copy: leftOperand2).
	cfg b2 addInstruction: (leftAddInstruction := instructionFactory add: copy3 to: copy4).

	"And this block will never be executed"
	cfg b3 addInstruction: (copy5 := instructionFactory copy: rightOperand1).
	cfg b3 addInstruction: (copy6 := instructionFactory copy: rightOperand2).
	cfg b3 addInstruction: (rightAddInstruction := instructionFactory add: copy5 to: copy6).

	^ cfg
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: onLeft [

	| copy1 copy2 jump cfg |
	copy1 := instructionFactory copy: 1.
	copy2 := instructionFactory copy: 2.
	
	"If we want to evaluate on the right, swap the arguments"
	onLeft ifFalse: [ | swapTemp |
		swapTemp := copy1.
		copy1 := copy2.
		copy2 := swapTemp.
	].
	
	jump := instructionFactory jumpIf:
		        (instructionFactory lessOrEquals: copy1 than: copy2).

	cfg := self setUpControlFlowGraphWithConditionalJump: jump.
	cfg b1 addInstruction: copy1.
	cfg b1 addInstruction: copy2.

	^ cfg
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConditionalEvaluationOnLeftAppliesFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	
	optimisation applyTo: cfg.
	
	"Check that the block on the left was visited to apply constant folding"
	self assert: (cfg b2 instructions at: 3) isCopy.
	self assert: (cfg b2 instructions at: 3) operand1 value equals: 7
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConditionalEvaluationOnLeftDoesNotApplyFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	
	optimisation applyTo: cfg.
	
	"Check that the block in the right was not visited to apply constant propagation"
	self assert: (cfg b2 instructions at: 3) == leftAddInstruction
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConditionalEvaluationOnRightAppliesFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	
	optimisation applyTo: cfg.
	
	"Check that the block on the left was visited to apply constant folding"
	self assert: (cfg b3 instructions at: 3) isCopy.
	self assert: (cfg b3 instructions at: 3) operand1 value equals: 11
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConditionalEvaluationOnRightDoesNotApplyFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	
	optimisation applyTo: cfg.
	
	"Check that the block in the right was not visited to apply constant propagation"
	self assert: (cfg b3 instructions at: 3) == rightAddInstruction
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingAddInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: 4.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 7 asDRValue
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfLeftNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: r and: 3.

	self assert: cfg instructions allButLast last isAdd.
	self
		assert: cfg instructions allButLast last operand1 operands first
		equals: r.
	self
		assert: cfg instructions allButLast last operand2
		equals: 3 asDRValue
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingAddOfRightNonConstants [

	| r cfg |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRAdd between: 3 and: r.

	self assert: cfg instructions allButLast last isAdd.
	self
		assert: cfg instructions allButLast last operand1
		equals: 3 asDRValue.
	self
		assert: cfg instructions allButLast last operand2 operands first
		equals: r
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingBitAndInSingleBasicBlock [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRBitAnd
		       between: 2r0101010110
		       and: 2r0101011111.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 2r0101010110 asDRValue
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroLeft [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: 0
		       and: (DRPhysicalRegister name: 'Rx').

	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroRight [

	| cfg |
	cfg := self
		       testConstantFoldingOf: DRMultiply
		       between: (DRPhysicalRegister name: 'Rx')
		       and: 0.
	self assertLastInstructionOf: cfg isCopyValue: 0
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRMultiply between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: 12
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: operand1 and: operand2 [

	| b0 b1 cfg copy1 copy2 |
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { operand1 asDRValue } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { operand2 asDRValue } result:  (DRTemporaryRegister id: 2)).
	b1 addInstruction: (instruction operands: { copy1. copy2 } result:  (DRTemporaryRegister id: 3)).

	optimisation applyTo: cfg.
	^ cfg
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingSubstractInSingleBasicBlock [

	| cfg |
	cfg := self testConstantFoldingOf: DRSubtract between: 3 and: 4.
	self assertLastInstructionOf: cfg isCopyValue: -1
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithConstants [

	| cfg |
	cfg := self testConstantFoldingOf: DRBitShift between: 2r1000 and: 1.

	self assert: cfg instructions allButLast last isCopy.
	self
		assert: cfg instructions allButLast last operand1
		equals: 2r100 asDRValue
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testFoldBitShiftWithNonLeftConstant [

	| cfg r lastInstruction |
	r := DRPhysicalRegister name: 'Parameter'.
	cfg := self testConstantFoldingOf: DRBitShift between: r and: 1.

	lastInstruction := cfg instructions allButLast last.
	self assert: lastInstruction isLogicalRightShift.
	self assert: lastInstruction operand1 operand1 equals: r.
	self assert: lastInstruction operand2 value equals: 1.
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testSCCPPropagationBothWithPhiFunction [

	| cfg phiInstruction |
	
	"Use the same values on the left and the right"
	rightOperand1 := leftOperand1.
	rightOperand2 := leftOperand2.
	
	cfg := self setUpControlFlowGraphWithConditionalJump: (instructionFactory jumpIf: (DRPhysicalRegister name: 'Parameter')).
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	"Since both paths are valid, the Phi function should not be replaced"
	optimisation applyTo: cfg.
	
	self assert: cfg b4 instructions allButLast last operand1 value equals: leftOperand1 + leftOperand2
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testSCCPPropagationLeftWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	self assert: cfg b4 instructions allButLast last operand1 value equals: 7
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testSCCPPropagationRightWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	self assert: cfg b4 instructions allButLast last operand1 value equals: 11
]
