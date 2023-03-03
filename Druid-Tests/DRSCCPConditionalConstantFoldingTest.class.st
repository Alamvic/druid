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
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft:
		       false.
	optimisation applyTo: cfg.

	"Check that the block in the right was not visited to apply constant propagation"
	self
		assert: (cfg b2 instructions at: 3)
		identicalTo: leftAddInstruction
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testConditionalEvaluationOnLeftReplacesConditionalJump [

	"This test asserts that SCCP optimises only the blocks that are used.
	Here the constants in the conditional jump are propagated, the algorithm knows that the block2 b2 will not be called because the condition is always false.
	Only the b3 block is optimized."

	| cfg originalConditionalJump |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.

	originalConditionalJump := cfg b1 endInstruction.
	optimisation applyTo: cfg.

	"Check that the block on the left was visited to apply constant folding"
	self assert: cfg b1 endInstruction isNonConditionalJump.
	self assert: cfg b1 endInstruction target equals: originalConditionalJump trueBranch
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
	
	cfg := self setUpControlFlowGraphWithConditionalJump: (instructionFactory jumpIf: (DRPhysicalGeneralPurposeRegister name: 'Parameter')).
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
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationLeftWithPhiFunctionLeavesValidCFG [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: true.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	cfg validate.
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationRightWithPhiFunction [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	self assert: cfg b4 instructions allButLast last operand1 value equals: 11
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationRightWithPhiFunctionLevesValidCFG [

	| cfg phiInstruction |
	cfg := self setUpControlFlowGraphWithDecidableConditionalJumpOnLeft: false.
	cfg b4 addInstruction: (phiInstruction := instructionFactory phiWith: leftAddInstruction with: rightAddInstruction).

	optimisation applyTo: cfg.

	cfg validate.
]

{ #category : #'tests - conditional folding' }
DRSCCPConditionalConstantFoldingTest >> testSCCPPropagationWithFarAwayPhiFunctionFinishWithValidCFG [
	" |0| - - - - - - - - - -> |4| 
		  \-> |1| - - - -> |3| -^
					\-> |2| -^
	"

	| cfg b0 b1 b2 b3 b4 rx copy1 copy2 copyRx innerJump mainJump innerPhiInstruction mainPhiInstruction |
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b0.
	b0 addInstruction: (copy1 := instructionFactory copy: leftOperand1).
	b0 addInstruction: (copy2 := instructionFactory copy: leftOperand2).
	mainJump := instructionFactory jumpIf:
		        	(instructionFactory lessOrEquals: copy1 than: copy2).
	b0 addInstruction: mainJump operand1.
	b0 endInstruction: mainJump. 

	b1 := cfg newBasicBlock.
	mainJump falseBranch: b1.
	b1 addPredecessor: b0. 
	b1 addInstruction: (rx := instructionFactory copy: (DRPhysicalGeneralPurposeRegister name: 'Rx')).
	innerJump := instructionFactory jumpIf:
		        (instructionFactory lessOrEquals: copy1 than: rx).
	b1 addInstruction: innerJump operand1.
	b1 endInstruction: innerJump. 


	b2 := cfg newBasicBlock.
	innerJump falseBranch: b2.
	b2 addPredecessor: b1. 
	b2 addInstruction: (copyRx := instructionFactory copy: rx). 
	b2 endInstruction: DRNoop new. 
	
	b3 := cfg newBasicBlock.
	b2 jumpTo: b3.
	innerJump trueBranch: b3.
	b3 addPredecessor: b1. 
	b3 addInstruction: (innerPhiInstruction := instructionFactory phiWith: copyRx with: copy1).
	b3 endInstruction: DRNoop new. 

	b4 := cfg newBasicBlock.
	b3 jumpTo: b4.
	mainJump trueBranch: b4.
	b4 addPredecessor: b0. 
	b4 addInstruction: (mainPhiInstruction := instructionFactory phiWith: innerPhiInstruction with: copy1).
	b4 endInstruction: DRNoop new.

	cfg applyOptimisation: optimisation.
	
	self assertCollection: cfg blocks hasSameElements: { b0 . b4 }
]
