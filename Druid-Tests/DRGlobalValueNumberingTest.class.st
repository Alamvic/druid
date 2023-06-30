Class {
	#name : #DRGlobalValueNumberingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #tests }
DRGlobalValueNumberingTest >> testCommutativeOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg instructionFactory copy3 copy4 firstAddition secondAddition |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy4 to: copy3).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions includes: secondAddition) not.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testComplexEquivalentOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg instructionFactory copy0 copy2 constant2 firstAddition secondAddition |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy0 := instructionFactory copy: 0).
	constant2 := DRConstantValue new;
   value: 2.
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy0 to: constant2).
	cfg b1 addInstruction: (copy2 := instructionFactory copy: 2).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy0 to: copy2).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions includes: secondAddition) not.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsNotReplacedWhenBranchesMerge [
	"This test asserts whether operations in different branches are scoped differently"
			
	| cfg instructionFactory copy3 copy4 copy5 phi1 phi2 phi3 phi4 |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFGWithConditional.
	cfg b2 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b3 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b3 addInstruction: (copy5 := instructionFactory copy: 5).
	cfg b4 addInstruction: (phi1 := instructionFactory phiWith: copy3 with: copy3).
	cfg b4 addInstruction: (phi2 := instructionFactory phiWith: copy3 with: copy4).
	cfg b4 addInstruction: (phi3 := instructionFactory phiWith: copy3 with: copy5).
	cfg b4 addInstruction: (phi4 := instructionFactory phiWith: copy3 with: copy5).
	optimisation applyTo: cfg.
	self assert: ((cfg b4 instructions includes: phi1) xor: (cfg b4 instructions includes: phi2)).
	self assert: ((cfg b4 instructions includes: phi3) xor: (cfg b4 instructions includes: phi4)).
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsNotReplacedWhenBranchesSplit [
	"This test asserts whether operations in different branches are scoped differently"
			
	| cfg instructionFactory jump copy3 copy4 firstAddition secondAddition |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.

	jump := self newConditionalJump.
	cfg := self setUpCFGWithConditional: jump.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b2 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b3 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy4).
	optimisation applyTo: cfg.
	self assert: (cfg b2 instructions includes: firstAddition).
	self assert: (cfg b3 instructions includes: secondAddition).
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg instructionFactory copy3 copy4 firstAddition secondAddition |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy4).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions includes: secondAddition) not.
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentPhiFunctionsAreRemoved [
			
	| cfg instructionFactory copy1 copy2 copy3 copy4 phi1 phi2 |
	
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFGWithConditional.
	cfg b2 addInstruction: (copy1 := instructionFactory copy: 3).
	cfg b2 addInstruction: (copy2 := instructionFactory copy: 3).
	cfg b3 addInstruction: (copy3 := instructionFactory copy: 4).
	cfg b3 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b4 addInstruction: (phi1 := instructionFactory phiWith: copy3 with: copy1).
	cfg b4 addInstruction: (phi2 := instructionFactory phiWith: copy4 with: copy1).
	
	cfg initialBasicBlock controlFlowGraph: cfg.
	
	optimisation applyTo: cfg.
		
	self assert: ((cfg b4 instructions includes: phi2) xor: (cfg b4 instructions includes: phi1)).
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testInstructionOperandsStayLive [

	| cfg copy1 copy2 instructionFactory addition |

	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy1 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy2 := instructionFactory copy: 3).
	cfg b1 addInstruction: (addition := instructionFactory add: copy1 to: copy2).
	
	optimisation applyTo: cfg.
	
	addition validate
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testLoadInstructionOperandsAreNotDeteleted [

	| cfg instructionFactory load |
	cfg := self setUpCFG: 1.
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg b1 addInstruction: (load := instructionFactory load: 3).
	optimisation applyTo: cfg.
	
	self assert: load operands size = 1.
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testNonEquivalentOperationIsNotReplaced [
	"This test asserts whether or not non equivalent operations are left alone by the optimisation"
			
	| cfg instructionFactory copy3 copy4 copy5 firstAddition secondAddition |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (copy5 := instructionFactory copy: 5).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy5).
	optimisation applyTo: cfg.
	self assert: (cfg b1 instructions includes: firstAddition).
	self assert: (cfg b1 instructions includes: secondAddition).
	
	
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testPhiFunctionOperandsStayLive [
	"This test asserts whether operands of phi functions remain live after applying global value numbering"
			
	| cfg instructionFactory copy1 copy2 phi1 |
	
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFGWithConditional.
	cfg b2 addInstruction: (copy1 := instructionFactory copy: 3).
	cfg b3 addInstruction: (copy2 := instructionFactory copy: 4).
	cfg b4 addInstruction: (phi1 := instructionFactory phiWith: copy2 with: copy1).
	
	cfg initialBasicBlock controlFlowGraph: cfg.
	
	optimisation applyTo: cfg.
		
	phi1 validate.
]

{ #category : #tests }
DRGlobalValueNumberingTest >> testPhiOperandReplacement [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the optimisation"
			
	| cfg jump copy b0 phi b1 b2 b3 b4 |
	optimisation := DRGlobalValueNumbering new.
	
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b | copy := b copy: 1 ].
	b2 := cfg newBasicBlock.
	
	b0 jumpTo: b1.
	b1 jumpTo: b2.	
	"Add the phi function later, this needs the dominator tree and it cannot be computed until all blocks are plugged"
	phi := b2 phiWith: copy.
	jump := b2 jumpIf: phi.
	
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	
	b3 addPredecessor: b1.
	jump trueBranch: b3.
	
	b4 addPredecessor: b1.
	jump falseBranch: b4.
	
	optimisation applyTo: cfg.
	
	"The jump should still be valid afterwards"
	jump validate
]
