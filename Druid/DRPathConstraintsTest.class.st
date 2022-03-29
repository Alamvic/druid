Class {
	#name : #DRPathConstraintsTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRPathConstraintsTest >> previousOptimizations [

	^ { 
		  DRPhiSimplication new.
		  (DRDeadBlockElimination then: DRCleanControlFlow).
		  DRFailureCodeBasedTailDuplication new.
		  DRSCCP new.
		  DRCopyPropagation new.
		  DRDeadCodeElimination new }
]

{ #category : #initialization }
DRPathConstraintsTest >> setUp [

	super setUp.
	self configureInterpreter.
	optimisation := DRConditionalPathSplitting then: DRDeadPathElimination
]

{ #category : #building }
DRPathConstraintsTest >> testConditionalConstraintsArePropagated [

	| cfg blocks edge12 edge23 |
	cfg := self setUpCFG: 3.
	cfg b1 copy: 1 asDRValue.
	cfg generatePaths.

	blocks := cfg reversePostOrderBlocks allButFirst.
	edge12 := self anyOutgoingEdge: cfg for: blocks first.
	edge23 := self anyOutgoingEdge: cfg for: blocks second.
	self assert: edge12 constraints equals: edge23 constraints

]

{ #category : #building }
DRPathConstraintsTest >> testConditionalConstraintsAreStacked [

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
DRPathConstraintsTest >> testConditionalConstraintsInferenceFromBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThan.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. ">= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "< 10"
	self assert: (trueConstraint includes: (DRRegisterConstraint greaterOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterConstraint less: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalConstraintsInferenceFromBranchesInverted [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThanInverted.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "<= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	self assert: (trueConstraint includes: (DRRegisterConstraint lessOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterConstraint greater: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalConstraintsInferenceFromInnerBranches [

	| cfg edges trueConstraint falseConstraint trueExpectedConstraint falseExpectedConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.

	edges := cfg outgoingEdgesFor: (cfg blockById: 6).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"
	trueExpectedConstraint := DRRegisterConstraint
		                          reg: trueConstraint reg
		                          constraint:
		                          (DRIntersectionConstraint
			                           between: 10
			                           and: 20).
	falseExpectedConstraint := DRRegisterConstraint
		                           greaterOrEquals: falseConstraint reg
		                           than: 20.
	self assert: (trueConstraint includes: trueExpectedConstraint).
	self assert: (falseConstraint includes: falseExpectedConstraint)
]

{ #category : #building }
DRPathConstraintsTest >> testCopyConstantAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: 1 asDRValue.
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints size equals: 1
]

{ #category : #building }
DRPathConstraintsTest >> testCopyRegisterConstraint [

	| cfg edge constraint |
	cfg := self generateDruidIRFor: #primitiveReturnOne.

	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	constraint := edge constraintFor: 'ReceiverResultReg'.
	self assert: (constraint includes: (DRRegisterConstraint equals: constraint reg than: 1) ).
]

{ #category : #building }
DRPathConstraintsTest >> testCopyRegisterNotAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: (DRPhysicalRegister name: 'Rx').
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints isEmpty.
]

{ #category : #queries }
DRPathConstraintsTest >> testDeadPath [

	| cfg deadPath expectedTarget pathBlockIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	deadPath := cfg deadPaths associations first.
	pathBlockIds := deadPath value collect: [ :e | e destination id ].
	expectedTarget := self anyIncomingEdge: cfg for: (cfg blockById: 6).

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: pathBlockIds hasSameElements: { 7. 16. 1 }
]

{ #category : #optimization }
DRPathConstraintsTest >> testEliminateDoubleDeadPaths [

	| cfg lastBlocks lastPaths |
	cfg := self generateDruidIRFor: #primitiveDNA.

	3 timesRepeat: [ 
		optimisation applyTo: cfg.
		self optimize: cfg ].

	"TODO: cfg fullPaths"
	lastPaths := cfg incomingEdgesFor: cfg lastBasicBlock.
	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastPaths size equals: 4. "X>10 || 10<=X>7 || 7<=X>0 || 0<=X"
	self assert: lastBlocks size equals: 2.
]

{ #category : #optimization }
DRPathConstraintsTest >> testEliminateInnerDeadPaths [

	| cfg lastBlocks |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	optimisation applyTo: cfg.
	self optimize: cfg.

	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isCopy.
	self assert: lastBlocks first firstInstruction operand1 value equals: 17.
	self assert: lastBlocks second firstInstruction isCopy.
	self assert: lastBlocks second firstInstruction operand1 value equals: 99.
]

{ #category : #optimization }
DRPathConstraintsTest >> testEliminateOnlyDeadPaths [

	| cfg lastBlocks |
	cfg := self generateDruidIRFor: #primitiveMultipleReturnBlocks.
1halt.
	optimisation applyTo: cfg.
	self optimize: cfg.

	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isCopy.
	self assert: lastBlocks first firstInstruction operand1 value equals: 17.
	self assert: lastBlocks second firstInstruction isCopy.
	self assert: lastBlocks second firstInstruction operand1 value equals: 99.
]

{ #category : #optimization }
DRPathConstraintsTest >> testEliminateSimpleDeadPaths [

	| cfg lastBlocks |
	cfg := self generateDruidIRFor: #primitiveSandclock.

	2 timesRepeat: [ 
		optimisation applyTo: cfg.
		self optimize: cfg ].

	lastBlocks := cfg lastBasicBlock predecessors.

	self assert: cfg deadPaths isEmpty.
	self assert: lastBlocks first firstInstruction isPhiFunction. "One branch finishes in a phi"
	self assert: lastBlocks second firstInstruction isCopy.
	self
		assert: lastBlocks second firstInstruction operand1 value
		equals: 116 "Other branch harcodes the value"
]

{ #category : #building }
DRPathConstraintsTest >> testFirstIncomingInfoHasNoConstraints [

	| cfg edge |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	
	edge := self anyIncomingEdge: cfg for: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountBranching [

	| cfg |
	cfg := self setUpCFGWithConditional.
	cfg generatePaths.

	self assert: cfg edges size equals: 5
]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountLinear [

	| cfg |
	cfg := self setUpCFG: 3.
	cfg generatePaths.
	
	self assert: cfg edges size equals: 3.

]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountMerging [

	| cfg edges |
	cfg := self setUpCFGWithConditional.
	cfg newBasicBlockWith: [ :b | cfg b4 jumpTo: b ].
	cfg generatePaths.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
]

{ #category : #building }
DRPathConstraintsTest >> testJumpDoesNotAddConstraint [

	| cfg edge |
	cfg := self setUpCFG: 2.
	cfg generatePaths.
	
	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #building }
DRPathConstraintsTest >> testLinearContigousBlocksShareConstraints [

	| cfg blocks |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	cfg generatePaths.

	blocks := cfg reversePostOrderBlocks allButFirst.
	self
		assert: (self anyOutgoingEdge: cfg for: blocks first)
		equals: (self anyIncomingEdge: cfg for: blocks second)
]

{ #category : #queries }
DRPathConstraintsTest >> testManyDeadPaths [

	| cfg deadPaths deadTargetIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranches.

	deadPaths := cfg deadPaths.
	deadTargetIds := deadPaths keys collect: [ :e | e destination id ].

	self assert: deadPaths size equals: 2.
	self assertCollection: deadTargetIds hasSameElements: { 10. 6 }
]
