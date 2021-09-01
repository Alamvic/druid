Class {
	#name : #DRSCCPConstantFoldingTest,
	#superclass : #TestCase,
	#instVars : [
		'n',
		'optimisation',
		'rightAddInstruction',
		'instructionFactory',
		'leftAddInstruction'
	],
	#category : #'Druid-Tests'
}

{ #category : #running }
DRSCCPConstantFoldingTest >> setUp [

	super setUp.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRSCCP new
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
	cfg b2 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b2 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b2 addInstruction: (leftAddInstruction := instructionFactory add: copy3 to: copy4).

	"And this block will never be executed"
	cfg b3 addInstruction: (copy5 := instructionFactory copy: 5).
	cfg b3 addInstruction: (copy6 := instructionFactory copy: 6).
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

	self testConstantFoldingOf: DRAdd between: 3 and: 4 equalsTo: 7
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingBitAndInSingleBasicBlock [

	self testConstantFoldingOf: DRBitAnd between: 2r0101010110 and: 2r0101011111 equalsTo: 2r0101010110
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingBitShiftInSingleBasicBlock [

	self testConstantFoldingOf: DRBitShift between: 2r1000 and: 1 equalsTo: 2r100
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroLeft [

	self testConstantFoldingOf: DRMultiply between: 0 and: (DRPhysicalRegister name: 'Rx') equalsTo: 0
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyByZeroRight [

	self testConstantFoldingOf: DRMultiply between: (DRPhysicalRegister name: 'Rx') and: 0 equalsTo: 0
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingMultiplyInSingleBasicBlock [

	self testConstantFoldingOf: DRMultiply between: 3 and: 4 equalsTo: 12
]

{ #category : #helpers }
DRSCCPConstantFoldingTest >> testConstantFoldingOf: instruction between: operand1 and: operand2 equalsTo: expectedResult [

	| b0 b1 cfg copy1 copy2 |
	cfg := self setUpCFG: 2. 
	b0 := cfg b0.
	b1 := cfg b1. 

	b1 addInstruction: (copy1 := DRCopy operands: { operand1 asDRValue } result:  (DRTemporaryRegister id: 1)).
	b1 addInstruction: (copy2 := DRCopy operands: { operand2 asDRValue } result:  (DRTemporaryRegister id: 2)).
	b1 addInstruction: (instruction operands: { copy1. copy2 } result:  (DRTemporaryRegister id: 3)).

	optimisation applyTo: cfg.
	
	self assert: cfg instructions allButLast last operand1 equals: expectedResult asDRValue.
	self assert: cfg instructions allButLast last isCopy.
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testConstantFoldingSubstractInSingleBasicBlock [

	self testConstantFoldingOf: DRSubtract between: 3 and: 4 equalsTo: -1
]

{ #category : #tests }
DRSCCPConstantFoldingTest >> testSCCPPropagationBothWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithConditionalJump: (instructionFactory jumpIf: (DRPhysicalRegister name: 'Parameter')).
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	"Since both paths are valid, the Phi function should not be replaced"
	optimisation applyTo: cfg.
	
	self assert: cfg b4 instructions allButLast last == phiInstruction
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
