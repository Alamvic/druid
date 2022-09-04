Class {
	#name : #DRBasicBlockTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRBasicBlockTest >> testBreakBlockCreateNewBlocks [

	| cfg copy blocks|
	cfg := self setUpCFG: 1.
	copy := cfg b1 copy: 1.

	copy breakBasicBlock.
	cfg validate.

	blocks := cfg reversePostOrderBlocks allButFirst. "Ignore initial block"

	self assert: blocks size equals: 3.
	self assertBlock: blocks first jumpTo: blocks second.
	self assertBlock: blocks second jumpTo: blocks third.

]

{ #category : #tests }
DRBasicBlockTest >> testBreakBlockDivideInstructions [

	| cfg copy1 copy2 copy3 copy4 copy5 blocks |
	cfg := self setUpCFG: 1.
	copy1 := cfg b1 copy: 1.
	copy2 := cfg b1 copy: 2.
	copy3 := cfg b1 copy: 3.
	copy4 := cfg b1 copy: 4.
	copy5 := cfg b1 copy: 5.

	copy3 breakBasicBlock.
	cfg validate.

	blocks := cfg reversePostOrderBlocks allButFirst. "Ignore initial block"

	self assertBlock: blocks first hasSameInstructions: { copy1. copy2 }.
	self assertBlock: blocks second hasSameInstructions: { copy3 }.
	self assertBlock: blocks third hasSameInstructions: { copy4. copy5 }.
]

{ #category : #tests }
DRBasicBlockTest >> testBreakBlockWithPhis [

	| cfg copy blocks |
	cfg := self setUpCFGWithConditionalWithPhi.
	copy := cfg b4 copy: 4.

	copy breakBasicBlock.
	cfg validate.

	blocks := cfg reversePostOrderBlocks allButFirst. "Ignore initial block"

	self assert: blocks fourth instructions first isPhiFunction.
	self assert: blocks fifth instructions first equals: copy.
	self assert: blocks sixth instructions first isNoop.
]
