Class {
	#name : #DRComposedConditionalSplittingTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRComposedConditionalSplittingTest >> previousOptimizations [

	^ { 
		  DRCopyPropagation new.
		  DRCleanControlFlow new.
		  DRDeadCodeElimination new }
]

{ #category : #running }
DRComposedConditionalSplittingTest >> setUp [

	super setUp.

	optimisation := DRComposedConditionalSplitting new
]

{ #category : #tests }
DRComposedConditionalSplittingTest >> testSplitAnd [

	| cfg jump composedCondition newBlock newJump trueBlock falseBlock |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAndConditionOnSameVariable.
	
	jump := cfg allConditionalJumps first.
	composedCondition := jump condition.
	trueBlock := jump trueBranch.
	falseBlock := jump falseBranch.
	
	cfg applyOptimisation: optimisation.

	newBlock := cfg blocks last.
	newJump := newBlock endInstruction.
	
	self assert: jump condition equals: composedCondition operand1.
	self assert: jump trueBranch equals: newBlock.
	self assert: jump falseBranch equals: falseBlock.
	
	self assert: newJump condition equals: composedCondition operand2.
	self assert: newJump trueBranch equals: trueBlock.
	self assert: newJump falseBranch equals: falseBlock.

]

{ #category : #tests }
DRComposedConditionalSplittingTest >> testSplitMultipleComposedCondition [

	| cfg jump composedCondition trueBlock falseBlock newBlock1 newBlock2 newJump1 newJump2 |
	cfg := self generateDruidIRFor: #primitiveBranchingWithTripleAnd.

	jump := cfg allConditionalJumps first.
	composedCondition := jump condition.
	trueBlock := jump trueBranch.
	falseBlock := jump falseBranch.

	cfg applyOptimisation: optimisation.

	newBlock1 := cfg blocks allButLast last.
	newJump1 := newBlock1 endInstruction.
	newBlock2 := cfg blocks last.
	newJump2 := newBlock2 endInstruction.

	self assert: jump condition equals: composedCondition operand1 operand1.
	self assert: jump trueBranch equals: newBlock2.
	self assert: jump falseBranch equals: falseBlock.

	self assert: newJump1 condition equals: composedCondition operand2.
	self assert: newJump1 trueBranch equals: trueBlock.
	self assert: newJump1 falseBranch equals: falseBlock.
	
	self assert: newJump2 condition equals: composedCondition operand1 operand2.
	self assert: newJump2 trueBranch equals: newBlock1.
	self assert: newJump2 falseBranch equals: falseBlock
]

{ #category : #tests }
DRComposedConditionalSplittingTest >> testSplitMultipleComposedConditionLeftAssociated [

	| cfg jump composedCondition trueBlock falseBlock newBlock1 newBlock2 newJump1 newJump2 |
	cfg := self generateDruidIRFor: #primitiveBranchingWithTripleAndLeftAssociated.

	jump := cfg allConditionalJumps first.
	composedCondition := jump condition.
	trueBlock := jump trueBranch.
	falseBlock := jump falseBranch.

	cfg applyOptimisation: optimisation.

	newBlock1 := cfg blocks last.
	newJump1 := newBlock1 endInstruction.
	newBlock2 := cfg blocks allButLast last.
	newJump2 := newBlock2 endInstruction.

	self assert: jump condition equals: composedCondition operand1 operand1.
	self assert: jump trueBranch equals: newBlock1.
	self assert: jump falseBranch equals: falseBlock.

	self assert: newJump1 condition equals: composedCondition operand1 operand2.
	self assert: newJump1 trueBranch equals: newBlock2.
	self assert: newJump1 falseBranch equals: falseBlock.
	
	self assert: newJump2 condition equals: composedCondition operand2.
	self assert: newJump2 trueBranch equals: trueBlock.
	self assert: newJump2 falseBranch equals: falseBlock
]

{ #category : #tests }
DRComposedConditionalSplittingTest >> testSplitOr [

	| cfg jump composedCondition trueBlock falseBlock newBlock newJump |
	cfg := self generateDruidIRFor: #primitiveBranchingWithOrConditionOnSameVariable.
	
	jump := cfg allConditionalJumps first.
	composedCondition := jump condition.
	trueBlock := jump trueBranch.
	falseBlock := jump falseBranch.
	
	cfg applyOptimisation: optimisation.

	newBlock := cfg blocks last.
	newJump := newBlock endInstruction.
	
	self assert: jump condition equals: composedCondition operand1.
	self assert: jump trueBranch equals: trueBlock.
	self assert: jump falseBranch equals: newBlock .
	
	self assert: newJump condition equals: composedCondition operand2.
	self assert: newJump trueBranch equals: trueBlock.
	self assert: newJump falseBranch equals: falseBlock.

]

{ #category : #tests }
DRComposedConditionalSplittingTest >> testSplitSimpleCondition [

	| cfg previousBlocks |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAssigments.

	previousBlocks := cfg blocks.

	cfg applyOptimisation: optimisation.

	self assertCollection: cfg blocks hasSameElements: previousBlocks
]
