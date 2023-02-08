Class {
	#name : #DRTailDuplicationTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

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

	cfg applyOptimisation:
		((DRSCCP then: DRCopyPropagation then: DRDeadCodeElimination) then:
			 DRCleanControlFlow new).

	mergeBlocks := cfg mergeBlocks .
	mergeBlocks first tailDuplicate.
	cfg validate.

	"All non phi instructions in the frontier should depend on phis inside this block"
	frontierOfFrontiers := mergeBlocks third.

	self assert: (frontierOfFrontiers instructions third operands allSatisfy: [ :op |
		op isPhiFunction and: [ op basicBlock = frontierOfFrontiers ] ])
]
