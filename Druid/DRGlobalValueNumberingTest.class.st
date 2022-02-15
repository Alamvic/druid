Class {
	#name : #DRGlobalValueNumberingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRGlobalValueNumberingTest >> testEquivalentOperationIsReplacedByPreviousInstance [
	"This test asserts whether or not equivalent operations are correctly identified and replaced by the global value numbering algorithm"
			
	| cfg instructionFactory copy3 copy4 firstAddition secondAddition optimisation |
	instructionFactory := DRInstructionFactory new.
	optimisation := DRGlobalValueNumbering new.
	cfg := self setUpCFG: 1.
	cfg b1 addInstruction: (copy3 := instructionFactory copy: 3).
	cfg b1 addInstruction: (copy4 := instructionFactory copy: 4).
	cfg b1 addInstruction: (firstAddition := instructionFactory add: copy3 to: copy4).
	cfg b1 addInstruction: (secondAddition := instructionFactory add: copy3 to: copy4).
	DRGlobalValueNumbering applyTo: cfg.
	self assert: (cfg b1 instructions at: 2) isCopy.
	
	
]
