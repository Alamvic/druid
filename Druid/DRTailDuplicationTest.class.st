Class {
	#name : #DRTailDuplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldCreatePhiFunction [

	| cfg copy userBlock |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	userBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: copy ].
	cfg b4 jumpTo: userBlock.

	cfg b4 tailDuplicate.
	self generatePaths: cfg.
	
	self assert: userBlock firstInstruction isPhiFunction
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldReusePhiFunctions [

	| cfg copy closerUserBlock fartherUserBlock |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	closerUserBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: copy ].
	cfg b4 jumpTo: closerUserBlock.
	fartherUserBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: copy ].
	closerUserBlock jumpTo: fartherUserBlock.

	cfg b4 tailDuplicate.
	self generatePaths: cfg.

	self assert: closerUserBlock firstInstruction isPhiFunction.
	self deny: fartherUserBlock firstInstruction isPhiFunction.
	self assert: (closerUserBlock firstInstruction hasUser: fartherUserBlock firstInstruction)
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldCopyBlock [

	| cfg copy1 b1 pred1 pred2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := 1"
		copy1 := b copy: 1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	self
		deny: pred1 successor
		equals: pred2 successor
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldGenerateNewVariables [

	| cfg copy1 b1 pred1 pred2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := 1"
		copy1 := b copy: 1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	self
		deny: pred1 successor instructions first result
		equals: pred2 successor instructions first result
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldKeepInstructionDependenciesInPhiWithinBlock [

	| cfg copy1 copy2 b1 pred1 pred2 comingFrom1 comingFrom2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlockWith: [ :b | comingFrom1 := b copy: 1 ].
	pred2 := cfg newBasicBlockWith: [ :b | comingFrom2 := b copy: 2 ].
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := Phi(comingFrom1, comingFrom2)"
		copy1 := b phiWith: comingFrom1 with: comingFrom2.
		"R1 := R0"
		copy2 := b copy: copy1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	"Validate the duplicated blocks"
	pred1 successor validate.
	pred2 successor validate.
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldKeepInstructionDependenciesWithinBlock [

	| cfg copy1 copy2 b1 pred1 pred2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := 1"
		copy1 := b copy: 1.
		"R1 := R0"
		copy2 := b copy: copy1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	"Validate the duplicated blocks"
	pred1 successor validate.
	pred2 successor validate.
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldKeepInstructionUsersWithinBlock [

	| cfg copy1 copy2 b1 pred1 pred2 branch tailDuplicated |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := 1"
		copy1 := b copy: 1.
		"R1 := R0"
		copy2 := b copy: copy1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	branch := b1 jumpIf: copy2 to: cfg newBasicBlock ifFalseTo: cfg newBasicBlock.

	b1 tailDuplicate.
	tailDuplicated := pred2 successor.
	
	b1 validate.
	tailDuplicated validate.
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldKeepPhiFunctionDependencies [

	| cfg copy1 copy2 b1 pred1 pred2 comingFrom1 comingFrom2 tailDuplicated |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlockWith: [ :b | comingFrom1 := b copy: 1 ].
	pred2 := cfg newBasicBlockWith: [ :b | comingFrom2 := b copy: 2 ].
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := Phi(comingFrom1, comingFrom2)"
		copy1 := b phiWith: comingFrom1 with: comingFrom2.
		"R1 := R0"
		copy2 := b copy: copy1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.


	b1 tailDuplicate.
	tailDuplicated := pred2 successor.
	
	"Validate that the instructions in the predecessor have the right users in the subsequence phis"
	pred1 validate.
	pred2 validate.
	
	"Validate that the phi functions in the duplicated blocks have the right dependencies"
	b1 validate.
	tailDuplicated validate.
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldNotGenerateNewVariablesForReturnValue [

	| cfg copy1 b1 pred1 pred2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	b1 := cfg newBasicBlockWith: [ :b |
		"ReturnValue := 1"
		copy1 := b setReturnValue: 1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	self assert: pred1 successor instructions first isSetReturnValue.
	self assert: pred2 successor instructions first isSetReturnValue
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldSplitPhiFunctions [

	| cfg copy1 copy2 b1 pred1 pred2 comingFrom1 comingFrom2 tailDuplicated |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlockWith: [ :b | comingFrom1 := b copy: 1 ].
	pred2 := cfg newBasicBlockWith: [ :b | comingFrom2 := b copy: 2 ].
	b1 := cfg newBasicBlockWith: [ :b |
		"R0 := Phi(comingFrom1, comingFrom2)"
		copy1 := b phiWith: comingFrom1 with: comingFrom2.
		"R1 := R0"
		copy2 := b copy: copy1.
	].
	pred1 jumpTo: b1.
	pred2 jumpTo: b1.

	b1 tailDuplicate.
	
	self assert: b1 instructions first isPhiFunction.
	self assert: b1 instructions first operands size equals: 1.
	self assert: b1 instructions first operands first equals: comingFrom1.
	
	tailDuplicated := pred2 successor.
	self assert: tailDuplicated instructions first isPhiFunction.
	self assert: tailDuplicated instructions first operands size equals: 1.
	self assert: tailDuplicated instructions first operands first equals: comingFrom2.
]
