Class {
	#name : #DRBasicBlockTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #assertion }
DRBasicBlockTest >> assertPath: path equalsIds: associationIds [

	| actualIds |
	actualIds := path collect: [ :edge | edge source id -> edge destination id ].
	self assert: actualIds equals: associationIds
]

{ #category : #break }
DRBasicBlockTest >> testBreakBlockCreateNewBlocks [

	| cfg copy blocks|
	cfg := self setUpCFG: 1.
	copy := cfg b1 copy: 1.

	copy breakBasicBlock.
	cfg validate.

	blocks := cfg reversePostOrderBlocks allButFirst. "Ignore initial block"

	self assert: blocks size equals: 4.
	self assertBlock: blocks first jumpsTo: blocks second.
	self assertBlock: blocks second jumpsTo: blocks third.

]

{ #category : #break }
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

{ #category : #break }
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

{ #category : #pathAt }
DRBasicBlockTest >> testPathAt [

	| cfg |
	cfg := self setUpCFGWithConditional.
	cfg generatePaths.
		
	self assertPath: (cfg pathAt: 1) equalsIds: {-1 -> 1 . 1 -> 2 . 2 -> 4}.
	self assertPath: (cfg pathAt: 2) equalsIds: {-1 -> 1 . 1 -> 3 . 3 -> 4}.

]
