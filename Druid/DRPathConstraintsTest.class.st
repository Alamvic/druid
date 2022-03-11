Class {
	#name : #DRPathConstraintsTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #assertions }
DRPathConstraintsTest >> assert: aConstraint includes: includedConstraint [

	self assert: (aConstraint includes: includedConstraint)
]

{ #category : #assertions }
DRPathConstraintsTest >> deny: aConstraint includes: includedConstraint [

	self deny: (aConstraint includes: includedConstraint)
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegConstraintTypeBothBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThan.
	self optimize: cfg.
	self generatePaths: cfg.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. ">= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "< 10"
	self assert: (trueConstraint includes: (DRRegisterConstraint greaterOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterConstraint less: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegConstraintTypeBothBranchesInverted [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThanInverted.
	self optimize: cfg.
	self generatePaths: cfg.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "<= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	self assert: (trueConstraint includes: (DRRegisterConstraint lessOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterConstraint greater: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegConstraintTypeInnerBranches [

	| cfg edges trueConstraint falseConstraint composedConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.
	self optimize: cfg.
	self generatePaths: cfg.

	edges := cfg outgoingEdgesFor: (cfg blockById: 6).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"
	composedConstraint := DRRegisterConstraint
		                      reg: trueConstraint reg
		                      constraint: (DRIntersectionConstraint between: 10 and: 20).
	self assert: trueConstraint includes: composedConstraint.
	self assert: falseConstraint includes: (DRRegisterConstraint greaterOrEquals: falseConstraint reg than: 20)
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegConstraintTypeInnerDeadBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.
	self optimize: cfg.
	self generatePaths: cfg.

	edges := cfg outgoingEdgesFor: (cfg blockById: 6).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "Dead"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	self assert: trueConstraint condition isDead.
	self assert: falseConstraint includes: (DRRegisterConstraint greater: falseConstraint reg than: 10)
]

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegConstraintTypeInnerX [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveSandClock.
	self optimize: cfg.
	self generatePaths: cfg.
1halt.
	edges := cfg outgoingEdgesFor: (cfg blockById: 6).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "Dead"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	self assert: trueConstraint condition isNil.
	self assert: falseConstraint includes: (DRRegisterConstraint greater: falseConstraint reg than: 10)
]

{ #category : #building }
DRPathConstraintsTest >> testConstraintsArePropagated [

	| cfg blocks edge12 edge23 |
	cfg := self setUpCFG: 3.
	cfg b1 copy: 1 asDRValue.
	self generatePaths: cfg.

	blocks := cfg reversePostOrderBlocks allButFirst.
	edge12 := cfg outgoingEdgeFor: blocks first.
	edge23 := cfg outgoingEdgeFor: blocks second.
	self assert: edge12 constraints equals: edge23 constraints

]

{ #category : #building }
DRPathConstraintsTest >> testConstraintsAreStacked [

	| cfg blocks edge12 edge23 |
	cfg := self setUpCFG: 3.
	cfg b1 copy: 1 asDRValue.
	cfg b2 copy: 2 asDRValue.
	self generatePaths: cfg.

	blocks := cfg reversePostOrderBlocks allButFirst.
	edge12 := cfg outgoingEdgeFor: blocks first.
	edge23 := cfg outgoingEdgeFor: blocks second.
	self assert: edge12 constraints size equals: 1.
	self assert: edge23 constraints size  equals: 2.
	

]

{ #category : #building }
DRPathConstraintsTest >> testCopyConstantAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: 1 asDRValue.
	self generatePaths: cfg.

	block := cfg firstBasicBlock.
	self assert: (cfg incomingEdgeFor: block) constraints isEmpty.
	self assert: (cfg outgoingEdgeFor: block) constraints size equals: 1
]

{ #category : #constraints }
DRPathConstraintsTest >> testCopyRegisterConstraint [

	| cfg edge constraint |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	self optimize: cfg.
	self generatePaths: cfg.

	edge := cfg outgoingEdgeFor: cfg firstBasicBlock.
	constraint := edge constraintFor: 'ReceiverResultReg'.
	self assert: (constraint includes: (DRRegisterConstraint equals: constraint reg than: 1) ).
]

{ #category : #building }
DRPathConstraintsTest >> testCopyRegisterNotAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: (DRPhysicalRegister name: 'Rx').
	self generatePaths: cfg.

	block := cfg firstBasicBlock.
	self assert: (cfg incomingEdgeFor: block) constraints isEmpty.
	self assert: (cfg outgoingEdgeFor: block) constraints isEmpty.
]

{ #category : #queries }
DRPathConstraintsTest >> testDeadPath [

	| cfg deadPath expectedTarget pathBlockIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.
	self optimize: cfg.
	self generatePaths: cfg.

	deadPath := cfg deadPaths associations first.
	pathBlockIds := deadPath value collect: [ :e | e destination id ].
	expectedTarget := cfg incomingEdgeFor: (cfg blockById: 6).

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: pathBlockIds hasSameElements: { 7. 16. 1 }
]

{ #category : #constraints }
DRPathConstraintsTest >> testEqualsConstraint [

	| constraint |
	constraint := DREqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 20).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #building }
DRPathConstraintsTest >> testFirstIncomingInfoHasNoConstraints [

	| cfg edge |
	cfg := self generateDruidIRFor: #emptyPrimitive.
	self optimize: cfg.
	self generatePaths: cfg.
	
	edge := cfg incomingEdgeFor: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountBranching [

	| cfg |
	cfg := self setUpCFGWithConditional.
	self generatePaths: cfg.

	self assert: cfg edges size equals: 5
]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountLinear [

	| cfg |
	cfg := self setUpCFG: 3.
	self generatePaths: cfg.
	
	self assert: cfg edges size equals: 3.

]

{ #category : #building }
DRPathConstraintsTest >> testGenerateExpectedEdgesCountMerging [

	| cfg edges |
	cfg := self setUpCFGWithConditional.
	cfg newBasicBlockWith: [ :b | cfg b4 jumpTo: b ].
	self generatePaths: cfg.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
]

{ #category : #constraints }
DRPathConstraintsTest >> testGreaterConstraint [

	| constraint |
	constraint := DRGreaterConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self deny: constraint includes: (DREqualsConstraint withValue: 9).
	
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 9).
	
	self deny: constraint includes: (DRUnionConstraint less: 5 greater: 15).
]

{ #category : #constraints }
DRPathConstraintsTest >> testGreaterOrEqualsConstraint [

	| constraint |
	constraint := DRGreaterOrEqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self deny: constraint includes: (DREqualsConstraint withValue: 9).
	self assert: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	self deny: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRPathConstraintsTest >> testIntersectionConstraint [

	| constraint |
	constraint := DRIntersectionConstraint between: 10 and: 20.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 15).
	self deny: constraint includes: (DREqualsConstraint withValue: 5).
	self deny: constraint includes: (DREqualsConstraint withValue: 25).
	
	self deny: constraint includes: (DRGreaterConstraint withValue: 15).
	self deny: constraint includes: (DRLessConstraint withValue: 15).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self assert: constraint includes: (DRIntersectionConstraint between: 11 and: 19).
	self deny: constraint includes: (DRIntersectionConstraint between: 9 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 10 and: 21).
	self deny: constraint includes: (DRIntersectionConstraint between: 9 and: 21).

	self deny: constraint includes: (DRUnionConstraint less: 9 greater: 21).
	self deny: constraint includes: (DRUnionConstraint less: 11 greater: 19).

]

{ #category : #building }
DRPathConstraintsTest >> testJumpDoesNotAddConstraint [

	| cfg edge |
	cfg := self setUpCFG: 2.
	self generatePaths: cfg.
	
	edge := cfg outgoingEdgeFor: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

]

{ #category : #constraints }
DRPathConstraintsTest >> testLessConstraint [

	| constraint |
	constraint := DRLessConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #constraints }
DRPathConstraintsTest >> testLessOrEqualsConstraint [

	| constraint |
	constraint := DRLessOrEqualsConstraint withValue: 10.
	
	self assert: constraint includes: (DREqualsConstraint withValue: 10).
	self deny: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self assert: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRGreaterConstraint withValue: 10).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self deny: constraint includes: (DRNotEqualsConstraint withValue: 10).
]

{ #category : #building }
DRPathConstraintsTest >> testLinearContigousBlocksShareConstraints [

	| cfg blocks |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	self optimize: cfg.
	self generatePaths: cfg.
	
	blocks := cfg reversePostOrderBlocks allButFirst.
	self assert: (cfg outgoingEdgeFor: blocks first) equals: (cfg incomingEdgeFor: blocks second).

]

{ #category : #queries }
DRPathConstraintsTest >> testManyDeadPaths [

	| cfg deadPaths deadTargetIds |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranches.
	self optimize: cfg.
	self generatePaths: cfg.

	deadPaths := cfg deadPaths.
	deadTargetIds := deadPaths keys collect: [ :e | e destination id ].

	self assert: deadPaths size equals: 2.
	self assertCollection: deadTargetIds hasSameElements: { 10. 6 }
]

{ #category : #constraints }
DRPathConstraintsTest >> testNotEqualsConstraint [

	| constraint |
	constraint := DRNotEqualsConstraint withValue: 10.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 10).
	self assert: constraint includes: (DREqualsConstraint withValue: 11).
	self assert: constraint includes: (DREqualsConstraint withValue: 9).
	
	self deny: constraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self deny: constraint includes: (DRLessOrEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRGreaterConstraint withValue: 10).
	self assert: constraint includes: (DRGreaterConstraint withValue: 11).
	self deny: constraint includes: (DRGreaterConstraint withValue: 9).
	
	self assert: constraint includes: (DRLessConstraint withValue: 10).
	self assert: constraint includes: (DRLessConstraint withValue: 9).
	self deny: constraint includes: (DRLessConstraint withValue: 11).
	
	self assert: constraint includes: (DRNotEqualsConstraint withValue: 10).
	
	self assert: constraint includes: (DRIntersectionConstraint between: 5 and: 10).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self assert: constraint includes: (DRIntersectionConstraint between: 10 and: 15).
	
	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 10).
	self deny: constraint includes: (DRUnionConstraint less: 15 greater: 20).
]

{ #category : #constraints }
DRPathConstraintsTest >> testUnionConstraint [

	| constraint |
	constraint := DRUnionConstraint less: 10 greater: 20.
	
	self deny: constraint includes: (DREqualsConstraint withValue: 15).
	self assert: constraint includes: (DREqualsConstraint withValue: 5).
	self assert: constraint includes: (DREqualsConstraint withValue: 25).
	
	self deny: constraint includes: (DRGreaterConstraint withValue: 15).
	self assert: constraint includes: (DRGreaterConstraint withValue: 20).
	self deny: constraint includes: (DRGreaterConstraint withValue: 5).
	
	self deny: constraint includes: (DRLessConstraint withValue: 15).
	self assert: constraint includes: (DRLessConstraint withValue: 5).
	self deny: constraint includes: (DRLessConstraint withValue: 25).

	self deny: constraint includes: (DRNotEqualsConstraint withValue: 15).
	
	self deny: constraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self deny: constraint includes: (DRIntersectionConstraint between: 5 and: 15).
	self deny: constraint includes: (DRIntersectionConstraint between: 15 and: 25).
	self assert: constraint includes: (DRIntersectionConstraint between: 20 and: 25).
	self assert: constraint includes: (DRIntersectionConstraint between: 5 and: 10).

	self assert: constraint includes: (DRUnionConstraint less: 10 greater: 20).
	self deny: constraint includes: (DRUnionConstraint less: 11 greater: 19).
	self assert: constraint includes: (DRUnionConstraint less: 9 greater: 21).

]
