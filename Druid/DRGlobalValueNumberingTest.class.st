Class {
	#name : #DRGlobalValueNumberingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRGlobalValueNumberingTest >> testCommutativeOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg instructionFactory copy3 copy4 firstAddition secondAddition optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy4 to: copy3).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions at: 4) isCopy.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsNotReplacedWhenBranchesMerge [
	"This test asserts whether operations in different branches are scoped differently"
			
	| cfg instructionFactory jump copy3 copy4 copy5 phi1 phi2 phi3 phi4 optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	jump := DRConditionalJump operands: { DRConstantValue value: true }.
	cfg := self setUpCFGWithConditional: jump.
	cfg b2 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b2 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b3 addInstruction: (copy5 := instructionFactory copy: 5).
	cfg b4 addInstruction: (phi1 := instructionFactory phiWith: copy3 with: copy3).
	cfg b4 addInstruction: (phi2 := instructionFactory phiWith: copy3 with: copy4).
	cfg b4 addInstruction: (phi3 := instructionFactory phiWith: copy3 with: copy5).
	cfg b4 addInstruction: (phi4 := instructionFactory phiWith: copy3 with: copy5).
	optimisation applyTo: cfg.
	self assert: (cfg b4 instructions findFirst: phi1) == 0.
	self assert: (cfg b4 instructions findFirst: phi2) ~~ 0.
	self assert: (cfg b4 instructions findFirst: phi3) ~~ 0.
	self assert: (cfg b4 instructions findFirst: phi4) == 0.
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsNotReplacedWhenBranchesSplit [
	"This test asserts whether operations in different branches are scoped differently"
			
	| cfg instructionFactory jump copy3 copy4 firstAddition secondAddition optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	jump := DRConditionalJump operands: { DRConstantValue value: true }.
	cfg := self setUpCFGWithConditional: jump.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b2 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b3 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy4).
	optimisation applyTo: cfg.
	self assert: (cfg b2 instructions at: 1) isCopy not.
	self assert: (cfg b3 instructions at: 1) isCopy not.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg instructionFactory copy3 copy4 firstAddition secondAddition optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy4).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions at: 4) isCopy.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testNonEquivalentOperationIsNotReplaced [
	"This test asserts whether or not non equivalent operations are left alone by the optimisation"
			
	| cfg instructionFactory copy3 copy4 copy5 optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (copy5 := instructionFactory copy: 5).
	cfg b1 addInstruction: (instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (instructionFactory add: copy3 to: copy5).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions at: 4) isCopy not.
	self assert: (cfg b1 instructions at: 5) isCopy not.
	
	
]
