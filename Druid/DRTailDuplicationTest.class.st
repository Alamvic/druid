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
	cfg validate.

	self assert: userBlock firstInstruction isPhiFunction.
	self
		assertCollection: copy users
		hasSameElements: { userBlock firstInstruction }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldCreatePhiFunctionWithMultipleVariables [

	"|0| -> |1| -> |2| --> |4| -> |7| -> |8|
             \ -> |3| --> |5| ---^
                     \ -> |6| ---^             
	"

	| cfg b5 b6 copy b7 b8 |
	cfg := self setUpCFGWithConditional.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	b8 := cfg newBasicBlock.

	cfg b3 jumpIfTrueTo: b5 ifFalseTo: b6.
	cfg b4 jumpTo: b7.
	b5 jumpTo: b7.
	b6 jumpTo: b7.
	b7 jumpTo: b8.
	
	copy := b7 copy: 1.
	b8 add: 1 to: copy.

	b7 tailDuplicate.
	cfg validate.

	self assert: b8 predecessors size equals: 3.
	self assert: b8 firstInstruction isPhiFunction.
	self assert: b8 firstInstruction operands size equals: 3.
	

]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldCreatePhiFunctionsChain [

	| cfg copy userBlock leftBlock rightBlock |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.

	leftBlock := cfg newBasicBlock.
	rightBlock := cfg newBasicBlock.

	cfg b4	 jumpIfTrueTo: leftBlock ifFalseTo: rightBlock.

	userBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: copy ].
	leftBlock jumpTo: userBlock.
	rightBlock jumpTo: userBlock.

	cfg b4 tailDuplicate.
	cfg validate.

	self assert: userBlock firstInstruction isPhiFunction. "Phi of 2 phis"
	self assert: userBlock firstInstruction operand1 isPhiFunction.
	self assert: userBlock firstInstruction operand2 isPhiFunction.
	self
		assertCollection: copy users
		hasSameElements: userBlock firstInstruction operands
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldMergePhiFunctions [

	"|0| -> |1| -> |2| --> |4| --> |6|
             \ -> |3| ----^       ^
                     \ -> |5| ----^             
	"


	| cfg b5 b6 r1 r2 phi newBlock |
	cfg := self setUpCFGWithConditional.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.

	cfg b4 removePredecessor: cfg b3.
	cfg b3 jumpIfTrueTo: cfg b4 ifFalseTo: b5.
	cfg b4 jumpTo: b6.
	b5 jumpTo: b6.

	r1 := cfg b4 copy: 1.
	r2 := b5 copy: 2.
	phi := b6 phiWith: r1 with: r2.

	cfg b4 tailDuplicate.
	cfg validate.
	newBlock := cfg blockById: 7.

	self assert: b6 phiFunctions size equals: 2.
	self assert: b6 firstInstruction operand2 isNullValue.
	self assertCollection: phi operands hasSameElements: { 
			r1.
			r2.
			newBlock firstInstruction }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldMergePhiFunctionsWithMultipleVariables [

	"|0| -> |1| -> |2| --> |4| -> |7|
             \       \ -> |5| ---^
              \ -> |3| ---^      ^
                     \ -> |6| ---^             
	"

	| cfg b5 b6 b7 copy1 copy2 copy3 phi newBlock |
	cfg := self setUpCFGWithConditional.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.

	cfg b2 jumpIfTrueTo: cfg b4 ifFalseTo: b5.
	cfg b3 jumpIfTrueTo: b5 ifFalseTo: b6.
	cfg b4 jumpTo: b7.
	b5 jumpTo: b7.
	b6 jumpTo: b7.

	copy1 := cfg b4 copy: 1.
	copy2 := b5 copy: 2.
	copy3 := b6 copy: 3.
	phi := b7 phiWithVariables: { copy1. copy2. copy3 }.

	b5 tailDuplicate.
	cfg validate.
	newBlock := cfg blockById: 8.

	self assert: b7 predecessors size equals: 4.
	self assertCollection: phi operands hasSameElements: { 
			copy1.
			copy2.
			copy3.
			newBlock firstInstruction }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldReusePhiFunctions [

	| cfg copy closerUserBlock fartherUserBlock fartherAdd |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	closerUserBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: copy ].
	cfg b4 jumpTo: closerUserBlock.
	fartherUserBlock := cfg newBasicBlockWith: [ :b | 
		                    fartherAdd := b add: 1 to: copy ].
	closerUserBlock jumpTo: fartherUserBlock.

	cfg b4 tailDuplicate.
	cfg validate.

	self assert: closerUserBlock firstInstruction isPhiFunction.
	self assert: fartherUserBlock firstInstruction equals: fartherAdd.
	self
		assertCollection: copy users
		hasSameElements: { closerUserBlock firstInstruction }.
	self
		assertCollection: closerUserBlock firstInstruction users
		hasSameElements: { 
				closerUserBlock instructions second.
				fartherUserBlock firstInstruction }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldCopyBlock [

	| cfg newBlock |
	cfg := self setUpCFGWithConditional.

	cfg b4 tailDuplicate.
	cfg validate.
	newBlock := cfg blockById: 5.

	self
		assertCollection: cfg b4 predecessors
		hasSameElements: { cfg b2 }.
	self
		assertCollection: newBlock predecessors
		hasSameElements: { cfg b3 }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldGenerateNewVariables [

	| cfg newBlock |
	cfg := self setUpCFGWithConditional.

	cfg b4 copy: 1.

	cfg b4 tailDuplicate.
	cfg validate.
	newBlock := cfg blockById: 5.

	self
		assert: newBlock instructions size
		equals: cfg b4 instructions size.
	self
		deny: cfg b4 firstInstruction result
		equals: newBlock firstInstruction result
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldKeepInstructionDependenciesInPhiWithinBlock [

	| cfg comingFrom1 comingFrom2 newBlock |
	cfg := self setUpCFGWithConditional.

	comingFrom1 := cfg b2 copy: 1.
	comingFrom2 := cfg b3 copy: 2.
	"Rn := Phi(comingFrom1, comingFrom2)"
	cfg b4 phiWith: comingFrom1 with: comingFrom2.

	cfg b4 tailDuplicate.
	cfg validate.
	newBlock := cfg blockById: 5.

	"On splitting, phis loses one argument"
	self
		assertCollection: comingFrom1 users
		hasSameElements: { cfg b4 firstInstruction }.
	self
		assertCollection: comingFrom2 users
		hasSameElements: { newBlock firstInstruction }
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
