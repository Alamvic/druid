Class {
	#name : #DRTailDuplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #tests }
DRTailDuplicationTest >> optimize: cfg [

	cfg applyOptimisation: (DRSCCP then: DRCopyPropagation).
	cfg applyOptimisation: (DRDeadCodeElimination then: DRCleanControlFlow).
	cfg applyOptimisation: DRPhiSimplication new
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithBaseOffsetStore [

	| cfg address duplicatedBlocks store1 store2 |
	cfg := self setUpCFGWithConditionalWithPhi.

	"Address has reference to instruction"
	address := DRBaseOffsetMemoryAddress new
		           base: cfg phi;
		           offset: 8 asDRValue;
		           yourself.
	cfg b4 storeSInt64: 87 asDRValue at: address.

	duplicatedBlocks := cfg b4 tailDuplicate.
	cfg validate.

	store1 := duplicatedBlocks first instructions second.
	store2 := duplicatedBlocks second instructions second.

	"Diff address instruction"
	self assert: store1 address equals: address.
	self deny: store1 address equals: store2 address.

	"Diff base (phis)"
	self deny: store1 address base equals: store2 address base
]

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

	| cfg b5 b6 copy b7 b8 add newBlocks phi1 phi2 |
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

	newBlocks := b7 tailDuplicate allButFirst.

	phi1 := (cfg blockById: 10) firstInstruction.
	phi2 := (cfg blockById: 8) firstInstruction.

	self assert: phi1 isPhiFunction.
	self
		assertCollection: phi1 operands
		hasSameElements: { 
				b7 firstInstruction.
				newBlocks last firstInstruction }.

	self assert: phi2 isPhiFunction.
	self
		assertCollection: phi2 operands
		hasSameElements: { 
				phi1.
				newBlocks first firstInstruction }.

	self assert: (add operands includes: phi2).
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
	b7 phiWithVariables: { copy1. copy2. copy3 }.

	b5 tailDuplicate.
	phi := b7 firstInstruction.
	newPhi := (cfg blockById: 7) firstInstruction.

	self assert: newPhi isPhiFunction.
	self 
		assertCollection: phi operands 
		hasSameElements: { copy1. newPhi. copy3 }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateBlockWithOusideUsersShouldMergePhiFunctionsWithMultipleVariablesUnbalanced [
	"|0| -> |1| -> |2| -> |4| -> |5| -> |7| -> |8| -> |9|
				 \ -> |3| --^	\					\ --------^
									 \ -> |6| ---------------^
                     
	"

	| cfg b5 b6 b7 b8 b9 copy1 newPhi user |
	cfg := self setUpCFGWithConditional.
	b5 := cfg newBasicBlock.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	b8 := cfg newBasicBlock.
	b9 := cfg newBasicBlock.

	cfg b4 jumpIfTrueTo: b5 ifFalseTo: b6.
	b6 jumpTo: b9.

	b5 jumpTo: b7.
	b7 jumpIfTrueTo: b9 ifFalseTo: b8.

	b8 jumpTo: b9.

	copy1 := cfg b4 copy: 1.
	user := b9 copy: copy1.

	cfg validate.
	cfg b4 tailDuplicate.

	"This phi has a same value coming from two predecedors"
	newPhi := b9 phiFunctions unique.

	self assert: newPhi operands size equals: 3.
	self assert: (newPhi operands last: 2) asSet size equals: 1.

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
	b6 phiWith: r1 with: r2.

	newBlock := cfg b4 tailDuplicate last.
	phi := b6 firstInstruction.
	newPhi := (cfg blockById: 6) firstInstruction.

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
DRTailDuplicationTest >> testTailDuplicateInsertingPhiInDegenerateControlFlow [
	"|0| -> |1| -> |2| -> |4| -> |5| -> |7| -------> |9|
				 \ -> |3| --^	\				\	          ^
									 \ ------------> |6| ----/
	"


	| cfg b6 b7 b9 copy1 newPhi user |
	cfg := self setUpCFGWithConditional.
	b6 := cfg newBasicBlock.
	b7 := cfg newBasicBlock.
	b9 := cfg newBasicBlock.

	cfg b4 jumpIfTrueTo: b7 ifFalseTo: b6.
	b6 jumpTo: b9.

	b7 jumpIfTrueTo: b9 ifFalseTo: b6.

	copy1 := cfg b4 copy: 1.
	user := b9 copy: copy1.

	cfg validate.
"1halt."
	cfg b4 tailDuplicate.

	"This phi has a same value coming from two predecedors"
	newPhi := b9 phiFunctions unique.

	self assert: newPhi operands first equals: (cfg blockById: 4) phiFunctions unique.
	self assert: newPhi operands second equals: (cfg blockById: 7) phiFunctions unique.

]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateOnLoop [

	| cfg duplicatedBlocks phi1 phi2 successors backjumpPredecessor |
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	"Duplicate loop header"
	duplicatedBlocks := (cfg blockById: 6) tailDuplicate.

	"Build 1 duplication"
	self assert: duplicatedBlocks size equals: 2.

	"Split phi"
	phi1 := duplicatedBlocks first phiFunctions first.
	phi2 := duplicatedBlocks second phiFunctions first.
	self assert: phi1 operands size equals: 1.
	self assert: phi2 operands size equals: 1.
	self deny: phi1 operands first equals: phi2 operands first.

	"Build phi of phis"
	successors := duplicatedBlocks first successors.
	self assert: successors size equals: 2.
	successors do: [ :succ | 
		self
			assertCollection: succ phiFunctions first operands
			hasSameElements: { phi1. phi2 } ].

	"Keep backjump on duplicated"
	backjumpPredecessor := duplicatedBlocks second predecessors anyOne.
	self assert: backjumpPredecessor endInstruction isBackJump
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

	| cfg inst firstMergeBlock secondMergeBlock newOperands phi copy mergeBlocks |
	cfg := self generateDruidIRFor: #primitiveSandclock.

	mergeBlocks := cfg mergeBlocks.
	firstMergeBlock := mergeBlocks first.
	secondMergeBlock := mergeBlocks second.

	inst := firstMergeBlock firstInstruction.
	phi := secondMergeBlock phiWith: inst with: inst.
	copy := secondMergeBlock copy: phi.

	newOperands := firstMergeBlock tailDuplicate collect: [ :b | b firstInstruction ].

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

	frontier := cfg blockById: 5.

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

	self assert: cfg b4 phiFunctions first users size equals: 1. "First phi used"

	self assert: b5 phiFunctions first users size equals: 1 "First master phi used (frontier of frontier)"
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierFirstBlock [

	| cfg duplicatedBlocks successors frontierOfFrontiers mergeBlocks |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new).

	mergeBlocks := cfg mergeBlocks.
	duplicatedBlocks := mergeBlocks first tailDuplicate.
	cfg validate.

	successors := duplicatedBlocks first successors.
	frontierOfFrontiers := mergeBlocks second.

	self assert: successors size equals: 2.

	self assert: (successors allSatisfy: [ :b | b firstInstruction isPhiFunction ]).

	self assert: frontierOfFrontiers phiFunctions size equals: 2.
	self
		assertCollection: frontierOfFrontiers phiFunctions second operands
		hasSameElements: (successors collect: [ :b | b firstInstruction ])
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierSecondBlock [

	| cfg duplicatedBlocks successors frontier mergeBlocks |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new).

	mergeBlocks := cfg mergeBlocks.
	mergeBlocks first tailDuplicate.
	duplicatedBlocks := (mergeBlocks second) tailDuplicate.
	cfg validate.

	successors := duplicatedBlocks first successors.
	frontier := mergeBlocks third.

	self assert: successors size equals: 2.

	self assert: (successors allSatisfy: [ :b | b phiFunctions size = 2 ]).

	self assert: frontier phiFunctions size equals: 3.
	self
		assertCollection: frontier instructions fourth operands
		hasSameElements: { frontier instructions second . frontier instructions third }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierWithMultipleBlocks [

	| cfg b5 b6 b7 b8 b9 b10 copy multipleFrontier copyLeft copyRight leftPhi rightPhi |
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

	
	copyLeft := cfg b2 copy: 1.
	copyRight := cfg b3 copy: 1.
	copy := cfg b4 copy: 1.
	
	leftPhi := b9 phiWithVariables: { copyRight . copy }.
	rightPhi :=  b8 phiWithVariables: { copyLeft . copy }.
	
	b10 copy: (b10 phiWithVariables: { leftPhi . copy . rightPhi }).

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

	| cfg b5 b6 b7 b8 b9 b10 copy b11 b12 firstFrontier secondFrontier user copyLeft copyRight leftPhi rightPhi leftPhiPhi rightPhiPhi |
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

	copyLeft := cfg b2 copy: 1.
	copyRight := cfg b3 copy: 1.
	copy := cfg b4	copy: 1.
	
	leftPhi := b9 phiWithVariables: { copyRight . copy }.
	rightPhi :=  b8 phiWithVariables: { copyLeft . copy }.
	
	leftPhiPhi := b11 phiWithVariables: { leftPhi . copy }.
	rightPhiPhi := b10 phiWithVariables: { rightPhi . copy }.

	user := b12 copy: (b12 phiWithVariables: { leftPhiPhi . rightPhiPhi }).	

	cfg b4 tailDuplicate.
	
	firstFrontier := { b7 . b8 . b9 }.
	secondFrontier := { b10 . b11 }.

	self assert: (firstFrontier allSatisfy: [ :b | b phiFunctions size = 1 ]).
	self assert: (secondFrontier allSatisfy: [ :b | b phiFunctions size = 1 ]).
	self assert: b12 firstInstruction isPhiFunction.
	self assert: (user operands includes: b12 firstInstruction) 
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithFrontierWithTwoBlocks [

	| cfg lastFrontier firstFrontier mergeBlocks |
	cfg := self generateDruidIRFor: #primitiveDNA.

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then: DRCleanControlFlow new).

	mergeBlocks := cfg mergeBlocks.

	"Duplicate both merge points"
	mergeBlocks first tailDuplicate.
	mergeBlocks second tailDuplicate.

	firstFrontier := mergeBlocks second successors..
	lastFrontier := mergeBlocks third.

	self assert: lastFrontier phiFunctions size equals: 3. "Own + 2 splits"
	self assert: (firstFrontier allSatisfy: [ :b | b phiFunctions size = 2 ]). "2 splits"

	self
		assertCollection: lastFrontier phiFunctions last operands
		hasSameElements: (firstFrontier collect: [ :b | b phiFunctions last ]).
		
	self
		assertCollection: lastFrontier instructions fourth operands
		hasSameElements: { lastFrontier phiFunctions second . lastFrontier phiFunctions third }
]

{ #category : #tests }
DRTailDuplicationTest >> testTailDuplicateWithPhiUsersInFrontier [

	| cfg frontierOfFrontiers mergeBlocks |
	cfg := self generateDruidIRFor: #primitiveDoubleInnerBranching.

	mergeBlocks := cfg mergeBlocks .
	mergeBlocks first tailDuplicate.
	cfg validate.

	"All non phi instructions in the frontier should depend on phis inside this block"
	frontierOfFrontiers := mergeBlocks third.

	self assert: (frontierOfFrontiers instructions third operands allSatisfy: [ :op |
		op isPhiFunction and: [ op basicBlock = frontierOfFrontiers ] ])
]
