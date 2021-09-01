Class {
	#name : #DRSCCPConditionalConstantFoldingTest,
	#superclass : #DRSCCPTest,
	#instVars : [
		'rightAddInstruction',
		'leftAddInstruction'
	],
	#category : #'Druid-Tests'
}

{ #category : #helpers }
DRSCCPConditionalConstantFoldingTest >> setUpControlFlowGraphWithConditionalJump: aJump [

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

{ #category : #helpers }
DRSCCPConditionalConstantFoldingTest >> setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: onLeft [

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

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testConditionalEvaluationOnLeftAppliesFolding [

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

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testConditionalEvaluationOnLeftDoesNotApplyFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	
	optimisation applyTo: cfg.
	
	"Check that the block in the right was not visited to apply constant propagation"
	self assert: (cfg b2 instructions at: 3) == leftAddInstruction
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testConditionalEvaluationOnRightAppliesFolding [

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

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testConditionalEvaluationOnRightDoesNotApplyFolding [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false. 
	Only the b3 block is optimized."

	| cfg |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	
	optimisation applyTo: cfg.
	
	"Check that the block in the right was not visited to apply constant propagation"
	self assert: (cfg b3 instructions at: 3) == rightAddInstruction
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationBothWithPhiFunction [

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

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationLeftWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	self assert: cfg b4 instructions allButLast last operand1 value equals: 7
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationRightWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	self assert: cfg b4 instructions allButLast last operand1 value equals: 11
]
