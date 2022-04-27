Class {
	#name : #DRTailDuplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithMultipleSuccessorsAndOnlyInsideUsersShouldKeepInstructionsDependecies [

	| cfg copy duplicatedBlocks lastBlock1 lastBlock2 |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	cfg b4 add: 1 to: copy.
	lastBlock1 := cfg newBasicBlock.
	lastBlock2 := cfg newBasicBlock.
	cfg b4 jumpIfTrueTo: lastBlock1 ifFalseTo: lastBlock2 .

	duplicatedBlocks := cfg b4 tailDuplicate.
	cfg validate.

	self assert: duplicatedBlocks size equals: 2.
	duplicatedBlocks do: [ :b | 
		self assert: (b instructions first users includes: b instructions second) ]
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldCreatePhiFunction [

	| cfg copy userBlock newPhi user |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	userBlock := cfg newBasicBlockWith: [ :b | user := b add: 1 to: copy ].
	cfg b4 jumpTo: userBlock.

	cfg b4 tailDuplicate.

	newPhi := userBlock predecessors first firstInstruction.

	self assert: newPhi isPhiFunction.
	self assertCollection: copy users hasSameElements: { newPhi }.

	self assert: user operands last equals: newPhi
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldCreatePhiFunctionWithMultipleVariables [

	"|0| -> |1| -> |2| --> |4| -> |7| -> |8|
             \ -> |3| --> |5| ---^
                     \ -> |6| ---^             
	"

	| cfg b5 b6 copy b7 b8 b9 add newPhi |
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
	add := b8 add: 1 to: copy.

	b7 tailDuplicate.
	cfg validate.

	b9 := cfg blockById: 9.
	newPhi := b9 firstInstruction.

	self assert: b9 predecessors size equals: 3.
	self assert: newPhi isPhiFunction.
	self assert: newPhi operands size equals: 3.
	self assertCollection: newPhi users hasSameElements: { add }.
	

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
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldMergePhiFunctionsWithMultipleVariables [

	"|0| -> |1| -> |2| --> |4| -> |7|
             \       \ -> |5| ---^
              \ -> |3| ---^      ^
                     \ -> |6| ---^             
	"

	| cfg b5 b6 b7 copy1 copy2 copy3 phi newPhi |
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
	newPhi := (cfg blockById: 8) firstInstruction.

	self assert: newPhi isPhiFunction.
	self 
		assertCollection: phi operands 
		hasSameElements: { copy1. newPhi. copy3 }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldReusePhiFunctions [

	| cfg copy closerUserBlock fartherUserBlock fartherAdd newPhi closerAdd |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	closerUserBlock := cfg newBasicBlockWith: [ :b | closerAdd := b add: 1 to: copy ].
	cfg b4 jumpTo: closerUserBlock.
	fartherUserBlock := cfg newBasicBlockWith: [ :b | 
		                    fartherAdd := b add: 1 to: copy ].
	closerUserBlock jumpTo: fartherUserBlock.

	cfg b4 tailDuplicate.
	cfg validate.
	
	newPhi := closerUserBlock predecessors first firstInstruction.

	self assert: newPhi isPhiFunction.
	self assert: closerUserBlock firstInstruction equals: closerAdd .
	self assert: fartherUserBlock firstInstruction equals: fartherAdd.
	self
		assertCollection: copy users
		hasSameElements: { newPhi }.
	self
		assertCollection: newPhi users
		hasSameElements: { closerAdd . fartherAdd  }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithPhiUsersShouldUpdateOperandToNewPhi [

	"|0| -> |1| -> |2| --> |4| --> |6|
             \ -> |3| ----^       ^
                     \ -> |5| ----^             
	"

	| cfg b5 b6 r1 r2 phi newBlock newPhi |
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

	newBlock := cfg b4 tailDuplicate last.
	newPhi := (cfg blockById: 7) firstInstruction.

	self assert: newPhi isPhiFunction.
	self assert: newPhi operands size equals: 2.
	self assertCollection: phi operands hasSameElements: { 
			newPhi.
			r2 }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithSingleSuccessorAndOnlyInsideUsersShouldKeepInstructionsDependecies [

	| cfg copy lastBlock duplicatedBlocks |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	cfg b4 add: 1 to: copy.
	lastBlock := cfg newBasicBlock.
	cfg b4 jumpTo: lastBlock.

	duplicatedBlocks := cfg b4 tailDuplicate.
	cfg validate.

	self assert: duplicatedBlocks size equals: 2.
	duplicatedBlocks do: [ :b | 
		self assert: (b instructions first users includes: b instructions second) ]
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithSingleSuccessorAndOutsideAndInsideUsersShouldKeepInstructionsDependecies [

	| cfg phi lastBlock duplicatedBlocks copy1 copy2 |
	cfg := self setUpCFGWithConditional.

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: copy1 with: copy2.
	cfg b4 add: 1 to: phi.
	lastBlock := cfg newBasicBlockWith: [ :b | b add: 1 to: phi ].
	cfg b4 jumpTo: lastBlock.

	duplicatedBlocks := cfg b4 tailDuplicate.
	cfg validate.

	self assert: duplicatedBlocks size equals: 2.
	duplicatedBlocks do: [ :b | 
		self assert: (b instructions first users includes: b instructions second).
		self assert: b phiFunctions first operands size equals: 1
		 ]
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
	cfg initialBasicBlock jumpIfTrueTo: pred1 ifFalseTo: pred2.
	
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
	cfg initialBasicBlock jumpIfTrueTo: pred1 ifFalseTo: pred2.
	
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
	cfg initialBasicBlock jumpIfTrueTo: pred1 ifFalseTo: pred2.

	b1 := cfg newBasicBlockWith: [ :b | 
		      pred1 jumpTo: b.
		      pred2 jumpTo: b.
		      "R0 := Phi(comingFrom1, comingFrom2)"
		      copy1 := b phiWith: comingFrom1 with: comingFrom2.
		      "R1 := R0"
		      copy2 := b copy: copy1 ].


	b1 tailDuplicate.
	tailDuplicated := pred2 successor.

	"Validate that the instructions in the predecessor have the right users in the subsequence phis"
	pred1 validate.
	pred2 validate.

	"Validate that the phi functions in the duplicated blocks have the right dependencies"
	b1 validate.
	tailDuplicated validate
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldNotGenerateNewVariablesForReturnValue [

	| cfg copy1 b1 pred1 pred2 |
	cfg := DRControlFlowGraph new.
	pred1 := cfg newBasicBlock.
	pred2 := cfg newBasicBlock.
	cfg initialBasicBlock jumpIfTrueTo: pred1 ifFalseTo: pred2.
	
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
	cfg initialBasicBlock jumpIfTrueTo: pred1 ifFalseTo: pred2.

	b1 := cfg newBasicBlockWith: [ :b | 
		      pred1 jumpTo: b.
		      pred2 jumpTo: b.
		      "R0 := Phi(comingFrom1, comingFrom2)"
		      copy1 := b phiWith: comingFrom1 with: comingFrom2.
		      "R1 := R0"
		      copy2 := b copy: copy1 ].

	b1 tailDuplicate.

	self assert: b1 instructions first isPhiFunction.
	self assert: b1 instructions first operands size equals: 1.
	self assert: b1 instructions first operands first equals: comingFrom1.

	tailDuplicated := pred2 successor.
	self assert: tailDuplicated instructions first isPhiFunction.
	self
		assert: tailDuplicated instructions first operands size
		equals: 1.
	self
		assert: tailDuplicated instructions first operands first
		equals: comingFrom2
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateShouldWithPhiFunctionWithRepeatedOperands [

	| cfg inst firstMergeBlock secondMergeBlock newOperands phi copy |
	cfg := self generateDruidIRFor: #primitiveSandclock.
	cfg applyOptimization:
		(DRSCCP then: DRDeadCodeElimination then: DRCleanControlFlow).

	firstMergeBlock := cfg blockById: 8.
	secondMergeBlock := cfg blockById: 13.

	inst := firstMergeBlock firstInstruction.
	phi := secondMergeBlock phiWith: inst with: inst.
	copy := secondMergeBlock copy: phi.

	newOperands := firstMergeBlock tailDuplicate collect: [ :b | 
		               b firstInstruction ].

	secondMergeBlock phiFunctions do: [ :i | 
		self assert: i operands size equals: 2.
		self deny: i operand1 equals: i operand2 ].

	"Replace the phi with new one"
	self deny: copy operand1 equals: phi.
	self deny: (secondMergeBlock instructions includes: phi)
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierAsDirectSuccessor [

	| cfg copy b5 b6 b7 bocksBefore phiHolders duplicatedBlocks frontier |
	cfg := self setUpCFGWithConditional.

	copy := cfg b4 copy: 1.
	b5 := cfg newBasicBlockWith: [ :b | b copy: copy ].
	b6 := cfg newBasicBlockWith: [ :b | b copy: copy ].
	b7 := cfg newBasicBlockWith: [ :b | b copy: copy ].

	cfg b4 jumpIfTrueTo: b5 ifFalseTo: b6.
	b5 jumpTo: b6.
	b6 jumpTo: b7.

	bocksBefore := cfg blocks copy.
	duplicatedBlocks := cfg b4 tailDuplicate.

	phiHolders := cfg b4 successors.

	frontier := cfg blockById: 6.

	self assert: cfg blocks size equals: bocksBefore size + 3. "Duplicated + 2 phi holders"

	phiHolders do: [ :phiHolderBlock | 
		self assert: phiHolderBlock phiFunctions size equals: 1.
		self
			assertCollection: phiHolderBlock phiFunctions first operands
			hasSameElements: (duplicatedBlocks collect: [ :b | b firstInstruction ]) ].

	self assert: frontier phiFunctions size equals: 1.
	self
		assertCollection: frontier phiFunctions first operands
		hasSameElements: (phiHolders collect: [ :b | b firstInstruction ])
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierDominatedByItself [

	| cfg b5 copy1 copy2 phi1 phi2 |
	cfg := self setUpCFGWithConditional.

	"Remove predecessors because jumps add it..."
	cfg b4 removePredecessorAt: 2.
	cfg b4 removePredecessorAt: 1.
	b5 := cfg newBasicBlock.
	cfg b2 jumpIfTrueTo: cfg b3 ifFalseTo: cfg b4.
	cfg b3 jumpIfTrueTo: cfg b4 ifFalseTo: b5.
	cfg b4 jumpTo: b5.

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi1 := cfg b4 phiWith: copy1 with: copy2.
	phi2 := b5 phiWith: phi1 with: copy2.
	b5 copy: phi2.
	
	cfg b3 tailDuplicate.

	self assert: cfg b4 phiFunctions size equals: 1.	"NOT create master phi here (frontier)"
	self assert: b5 phiFunctions size equals: 2. "Create master phi here (frontier of frontier)"
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierFirstBlock [

	| cfg duplicatedBlocks successors frontier |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimization:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new).

	duplicatedBlocks := (cfg blockById: 8) tailDuplicate.
	cfg validate.

	successors := duplicatedBlocks first successors.
	frontier := cfg blockById: 11.

	self assert: successors size equals: 2.

	self assert: (successors allSatisfy: [ :b | b firstInstruction isPhiFunction ]).

	self assert: frontier phiFunctions size equals: 2.
	self
		assertCollection: frontier phiFunctions second operands
		hasSameElements: (successors collect: [ :b | b firstInstruction ])
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierSecondBlock [

	| cfg duplicatedBlocks successors frontier |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimization:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new).

	(cfg blockById: 8) tailDuplicate.
	duplicatedBlocks := (cfg blockById: 11) tailDuplicate.
	cfg validate.

	successors := duplicatedBlocks first successors.
	frontier := cfg blockById: 16.

	self assert: successors size equals: 2.

	self assert: (successors allSatisfy: [ :b | b phiFunctions size = 2 ]).

	self assert: frontier phiFunctions size equals: 3.
	self
		assertCollection: frontier instructions fourth operands
		hasSameElements: { frontier instructions second . frontier instructions third }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierWithMultipleBlocks [

	| cfg b5 b6 b7 b8 b9 b10 copy multipleFrontier |
	cfg := self setUpCFGWithConditional.

	"Remove predecessors because jumps add it..."
	cfg b4 removePredecessorAt: 2.
	cfg b4 removePredecessorAt: 1.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	b8 := cfg newBasicBlock.
	b9 := cfg newBasicBlock.
	b10 := cfg newBasicBlock.
	cfg b4 jumpIfTrueTo: b5 ifFalseTo: b6.
	b5 jumpIfTrueTo: b7 ifFalseTo: b8.
	b6 jumpIfTrueTo: b7 ifFalseTo: b9.
	b7 jumpTo: b10.
	b8 jumpTo: b10.
	b9 jumpTo: b10.
	cfg b2 jumpIfTrueTo: b8 ifFalseTo: cfg b4.
	cfg b3 jumpIfTrueTo: b9 ifFalseTo: cfg b4.

	copy := cfg b4	copy: 1.
	b10 copy: copy.	

	cfg b4 tailDuplicate.
	
	multipleFrontier := { b7 . b8 . b9 }.

	self assert: (multipleFrontier allSatisfy: [ :b | b phiFunctions size = 1 ]).
	self assert: b10 firstInstruction isPhiFunction.
	self 
		assertCollection: (b10 firstInstruction operands collect: [ :i | i basicBlock ]) 
		hasSameElements: multipleFrontier 
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierWithMultipleBlocksTwice [

	| cfg b5 b6 b7 b8 b9 b10 copy multipleFrontier b11 b12 |
	cfg := self setUpCFGWithConditional.

	"Remove predecessors because jumps add it..."
	cfg b4 removePredecessorAt: 2.
	cfg b4 removePredecessorAt: 1.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	b8 := cfg newBasicBlock.
	b9 := cfg newBasicBlock.
	b10 := cfg newBasicBlock.
	b11 := cfg newBasicBlock.
	b12 := cfg newBasicBlock.
	cfg b4 jumpIfTrueTo: b5 ifFalseTo: b6.
	b5 jumpIfTrueTo: b7 ifFalseTo: b8.
	b6 jumpIfTrueTo: b7 ifFalseTo: b9.
	b7 jumpIfTrueTo: b10 ifFalseTo: b11.
	b8 jumpTo: b10.
	b9 jumpTo: b11.
	b10 jumpTo: b12.
	b11 jumpTo: b12.
	cfg b2 jumpIfTrueTo: b8 ifFalseTo: cfg b4.
	cfg b3 jumpIfTrueTo: b9 ifFalseTo: cfg b4.

	copy := cfg b4	copy: 1.
	b12 copy: copy.	
1halt.
	cfg b4 tailDuplicate.
	
	multipleFrontier := { b7 . b8 . b9 }.

	self assert: (multipleFrontier allSatisfy: [ :b | b phiFunctions size = 1 ]).
	self assert: b10 firstInstruction isPhiFunction.
	self 
		assertCollection: (b10 firstInstruction operands collect: [ :i | i basicBlock ]) 
		hasSameElements: multipleFrontier 
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierWithTwoBlocks [

	| cfg lastFrontier firstFrontier |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimization:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then: DRCleanControlFlow new).

	(cfg blockById: 11) tailDuplicate.
	(cfg blockById: 8) tailDuplicate.

	firstFrontier := {cfg blockById: 17 . cfg blockById: 18}.
	lastFrontier := cfg blockById: 16.

	self assert: lastFrontier phiFunctions size equals: 3. "Own + 2 splits"
	self assert: (firstFrontier allSatisfy: [ :b | b phiFunctions size = 2 ]). "2 splits"

	self
		assertCollection: lastFrontier phiFunctions last operands
		hasSameElements: (firstFrontier collect: [ :b | b phiFunctions last ]).
		
	self
		assertCollection: lastFrontier instructions fourth operands
		hasSameElements: { lastFrontier phiFunctions second . lastFrontier phiFunctions third }
]
