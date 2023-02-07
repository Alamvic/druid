Class {
	#name : #DRPathGenerationTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #api }
DRPathGenerationTest >> optimize: cfg [

	cfg applyOptimisation:
		(DRSCCP then: DRCopyPropagation then: DRComposedConditionalSplitting).
	cfg applyOptimisation:
		(DRComposedConditionalSplitting then: DRBranchCollapse).
	cfg applyOptimisation:
		(DRDeadCodeElimination then: DRCleanControlFlow).
	cfg generatePaths
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsArePropagated [

	| cfg blocks edge12 edge23 |
	cfg := self setUpCFG: 3.
	cfg b1 copy: 1 asDRValue.
	cfg generatePaths.

	blocks := cfg reversePostOrderBlocks allButFirst.
	edge12 := self anyOutgoingEdge: cfg for: blocks first.
	edge23 := self anyOutgoingEdge: cfg for: blocks second.
	self assert: edge12 constraints equals: edge23 constraints

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsAreStacked [

	| cfg blocks edge12 edge23 |
	cfg := self setUpCFG: 3.
	cfg b1 copy: 1 asDRValue.
	cfg b2 copy: 2 asDRValue.
	cfg generatePaths.

	blocks := cfg reversePostOrderBlocks allButFirst.
	edge12 := self anyOutgoingEdge: cfg for: blocks first.
	edge23 := self anyOutgoingEdge: cfg for: blocks second.
	self assert: edge12 constraints size equals: 1.
	self assert: edge23 constraints size  equals: 2.
	

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsFromComplexConditionalOnDifferentRegs [

	| cfg edges trueNDomain trueMDomain firstFalseNDomain secondFalseNDomain secondFalseMDomain |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAndConditionOnDifferentVariables. "n ~= 2 AND m < 5"

	edges := cfg lastBasicBlock predecessors flatCollect: [ :b | cfg incomingEdgesFor: b ].
	trueNDomain := edges second constraintFor: 'ReceiverResultReg'.  "n ~= 2"
	trueMDomain := edges second constraintFor: 'Arg0Reg'.  "m < 5"
	firstFalseNDomain := edges first constraintFor: 'ReceiverResultReg'.  "n = 2"
	secondFalseNDomain := edges third constraintFor: 'ReceiverResultReg'.  "n ~= 2"
	secondFalseMDomain := edges third constraintFor: 'Arg0Reg'.  "m >= 5"

	self assert: (trueNDomain includes: (DRNotEqualsConstraint withValue: 2)).
	self assert: (trueMDomain includes: (DRLessConstraint withValue: 5)).
	self assert: (firstFalseNDomain includes: (DREqualsConstraint withValue: 2)).
	self assert: (secondFalseNDomain includes: (DRNotEqualsConstraint withValue: 2)).
	self assert: (secondFalseMDomain includes: (DRGreaterOrEqualsConstraint withValue: 5)).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsFromComplexConditionalOnSameReg [

	| cfg edges trueRegDomain firstFalseRegDomain secondFalseRegDomain |
	cfg := self generateDruidIRFor: #primitiveBranchingWithAndConditionOnSameVariable. "n ~= 2 AND n < 5"

	edges := cfg lastBasicBlock predecessors flatCollect: [ :b | cfg incomingEdgesFor: b ].
	trueRegDomain := edges second constraintFor: 'ReceiverResultReg'.  "n ~= 2 AND n < 5"
	firstFalseRegDomain := edges first constraintFor: 'ReceiverResultReg'.  "n = 2"
	secondFalseRegDomain := edges third constraintFor: 'ReceiverResultReg'.  "n >= 5"

	self assert: (trueRegDomain includes: (DRIntersectionConstraint constraints: {
		DRNotEqualsConstraint withValue: 2.
		DRLessConstraint withValue: 5
	})).
	
	self assert: (firstFalseRegDomain includes: (DREqualsConstraint withValue: 2)).
	self assert: (secondFalseRegDomain includes: (DRGreaterOrEqualsConstraint withValue: 5)).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranchIfCondition [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThan.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. ">= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "< 10"
	
	self assertConstraint: trueConstraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self assertConstraint: falseConstraint includes: (DRLessConstraint withValue: 10).
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranchIfConditionInverted [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThanInverted.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "<= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	
	self assertConstraint: trueConstraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self assertConstraint: falseConstraint includes: (DRGreaterConstraint withValue: 10).
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranchIfTrueWithFalse [

	| cfg jump trueEdge falseEdge |
	cfg := self generateBasicDruidIRFor: #primitiveFalseIfTrue.
	cfg applyOptimisation: DRCleanControlFlow new.
	cfg generatePaths.

	jump := cfg firstBasicBlock lastInstruction.
	trueEdge := (cfg edgesFrom: cfg firstBasicBlock to: jump trueBranch) first.
	falseEdge := (cfg edgesFrom: cfg firstBasicBlock to: jump falseBranch) first.

	self assert: trueEdge isDead.
	self deny: falseEdge isDead.
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranchIfTrueWithTrue [

	| cfg jump trueEdge falseEdge |
	cfg := self generateBasicDruidIRFor: #primitiveTrueIfTrue.
	cfg applyOptimisation: DRCleanControlFlow new.
	cfg generatePaths.

	jump := cfg firstBasicBlock lastInstruction.
	trueEdge := (cfg edgesFrom: cfg firstBasicBlock to: jump trueBranch) first.
	falseEdge := (cfg edgesFrom: cfg firstBasicBlock to: jump falseBranch) first.

	self deny: trueEdge isDead.
	self assert: falseEdge isDead.
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromCopy [

	| cfg copy edge constraint |
	cfg := self setUpCFG: 2.
	
	copy := cfg b1 copy: 1.	
	cfg generatePaths.

	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	constraint := edge constraintFor: copy result name.

	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 1).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromCopyOfCopy [

	| cfg copy1 copy2 edge constraint |
	cfg := self setUpCFG: 2.
	
	copy1 := cfg b1 copy: 1.
	copy2 := cfg b1 copy: copy1.
	cfg generatePaths.

	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	constraint := edge constraintFor: copy2 result name.
	
	self assertConstraint: constraint includes: (DREqualsConstraint withValue: 1).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromInnerBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.

	edges := cfg incomingEdgesFor: (cfg blockById: 12).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"

	self
		assertConstraint: trueConstraint
		includes: (DRIntersectionConstraint between: 10 and: 20).
	self
		assertConstraint: falseConstraint
		includes: (DRGreaterOrEqualsConstraint withValue: 20)
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromLoop [

	| cfg edges firstPhiConstraint loopedPhiConstraint sumConstraint |
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	"Go out from loop"
	edges := cfg incomingEdgesFor: (cfg blockById: 5).

	"If never go inside the loop"
	firstPhiConstraint := edges first constraintFor: 'R5'.
	"If enter and then go out from loop"
	loopedPhiConstraint := edges second constraintFor: 'R5'.
	sumConstraint := edges second constraintFor: 'R10'.

	self assert: firstPhiConstraint isEmpty.
	self assertConstraint: loopedPhiConstraint includes: (DRGreaterOrEqualsConstraint withValue: 15).
	self assert: sumConstraint isNotNil
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromPhi [

	| cfg edges constraints |
	cfg := self setUpCFGWithConditionalWithPhi.
	cfg b4 jumpTo: cfg newBasicBlock.
	cfg generatePaths.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	constraints := edges collect: [ :e | 
		               e constraintFor: cfg phi result name ].

	self assert: edges size equals: 2.
	self
		assertConstraint: constraints first
		includes: (DREqualsConstraint withValue: 1).
	self
		assertConstraint: constraints second
		includes: (DREqualsConstraint withValue: 2)
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromPhiWithNull [

	| cfg copy1 copy2 phi edges constraints |
	cfg := self setUpCFGWithConditional.

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: copy1.
	cfg b4 jumpTo: cfg newBasicBlock.
	cfg generatePaths.

	edges := cfg incomingEdgesFor:  cfg lastBasicBlock.
	constraints := edges collect: [ :e | e constraintFor: phi result name ] .

	self assertConstraint: constraints first includes: (DREqualsConstraint withValue: 1).
	self assert: constraints second isNil.

]

{ #category : #building }
DRPathGenerationTest >> testCopyConstantAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: 1 asDRValue.
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints size equals: 1
]

{ #category : #building }
DRPathGenerationTest >> testCopyRegisterNotAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: (DRPhysicalGeneralPurposeRegister name: 'Rx').
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints isEmpty.
]

{ #category : #building }
DRPathGenerationTest >> testFirstIncomingInfoHasNoConstraints [

	| cfg edge |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	
	edge := self anyIncomingEdge: cfg for: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #building }
DRPathGenerationTest >> testGenerateExpectedEdgesCountBranching [

	| cfg |
	cfg := self setUpCFGWithConditional.
	cfg generatePaths.

	self assert: cfg edges size equals: 5
]

{ #category : #building }
DRPathGenerationTest >> testGenerateExpectedEdgesCountLinear [

	| cfg |
	cfg := self setUpCFG: 3.
	cfg generatePaths.
	self assert: cfg edges size equals: 3.

]

{ #category : #building }
DRPathGenerationTest >> testGenerateExpectedEdgesCountLoop [

	| cfg edges |
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.
	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
]

{ #category : #building }
DRPathGenerationTest >> testGenerateExpectedEdgesCountMerging [

	| cfg edges |
	cfg := self setUpCFGWithConditional.
	cfg newBasicBlockWith: [ :b | cfg b4 jumpTo: b ].
	cfg generatePaths.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
]

{ #category : #'dead-path' }
DRPathGenerationTest >> testInferenceDeadPaths [

	| cfg deadPath expectedTarget pathBlockIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	deadPath := cfg deadPaths associations first.
	pathBlockIds := deadPath value collect: [ :e | e destination id ].
	expectedTarget := self anyOutgoingEdge: cfg for: (cfg firstBasicBlock).

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: pathBlockIds hasSameElements: { 14. 12 . 28 }
]

{ #category : #'dead-path' }
DRPathGenerationTest >> testInferenceDeadPathsFromPhi [

	| cfg deadPaths sourceBlockIds condition |
	cfg := self setUpCFGWithConditionalWithPhi.
	condition := cfg b4 equals: cfg phi to: 1 asDRValue.
	cfg b4
		jumpIf: condition
		to: cfg newBasicBlock
		ifFalseTo: cfg newBasicBlock.
	cfg generatePaths.

	deadPaths := cfg deadPaths keys.
	sourceBlockIds := deadPaths collect: [ :e | e source id ].

	"As the condition is exclusive (XOR) each dead path comes from a different branch"
	self assertCollection: sourceBlockIds hasSameElements: { 2 . 3 }
]

{ #category : #'dead-path' }
DRPathGenerationTest >> testInferenceManyDeadPaths [

	| cfg deadPaths deadTargetIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranches.
	deadPaths := cfg deadPaths.
	deadTargetIds := deadPaths keys collect: [ :e | e destination ].

	self assert: deadPaths size equals: 2.
	self assertCollection: deadTargetIds hasSameElements: cfg firstBasicBlock successors
]

{ #category : #building }
DRPathGenerationTest >> testJumpDoesNotAddConstraint [

	| cfg edge |
	cfg := self setUpCFG: 2.
	cfg generatePaths.
	
	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #building }
DRPathGenerationTest >> testLinearContigousBlocksShareEdge [

	| cfg blocks |
	cfg := self generateDruidIRFor: #primitiveReturnOne.

	blocks := cfg reversePostOrderBlocks allButFirst.
	self
		assert: (self anyOutgoingEdge: cfg for: blocks first)
		equals: (self anyIncomingEdge: cfg for: blocks second)
]

{ #category : #'dead-path' }
DRPathGenerationTest >> testUnmergeableConditionalBranchToSameTarget [

	| cfg b0 copy1 copy2 b1 condition |
	
	"If a block has a conditional branch to a single same target, it can only be merged and transformed to an unconditional jump if there is no Phi function depending on the branching.
	In this case, we should build two different paths."
	
	cfg := DRControlFlowGraphForTesting new. 
	
	b0 := cfg newBasicBlock.
	cfg initialBasicBlock jumpTo: b0.
	
	copy1 := b0 copy: 1.
	copy2 := b0 copy: 2.
	condition := b0 greater: (DRPhysicalGeneralPurposeRegister name: 'NO SABES LOCO') than: 2.
	b1 := cfg newBasicBlock.
	b0 jumpIf: condition to: b1 ifFalseTo: b1.
	b1 phiWith: copy1 with: copy2.
	
	"Kill the condition boolean value, we don't care about it"
	"condition result: DRNoRegister new."
	cfg generatePaths.


	self assert: cfg deadPaths isEmpty
]