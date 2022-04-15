Class {
	#name : #DRPathGenerationTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #constraints }
DRPathGenerationTest >> testConditionalConstraintsArePropagated [

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
DRPathGenerationTest >> testConditionalConstraintsAreStacked [

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
DRPathGenerationTest >> testConditionalConstraintsInferenceFromBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThan.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. ">= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "< 10"
	self assert: (trueConstraint includes: (DRRegisterDomain greaterOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterDomain less: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathGenerationTest >> testConditionalConstraintsInferenceFromBranchesInverted [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThanInverted.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "<= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	self assert: (trueConstraint includes: (DRRegisterDomain lessOrEquals: trueConstraint reg than: 10) ).
	self assert: (falseConstraint includes: (DRRegisterDomain greater: falseConstraint reg than: 10) ).
]

{ #category : #constraints }
DRPathGenerationTest >> testConditionalConstraintsInferenceFromInnerBranches [

	| cfg edges trueConstraint falseConstraint trueExpectedConstraint falseExpectedConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.

	edges := cfg outgoingEdgesFor: (cfg blockById: 6).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"
	trueExpectedConstraint := DRRegisterDomain
		                          reg: trueConstraint reg
		                          constraint:
		                          (DRIntersectionConstraint
			                           between: 10
			                           and: 20).
	falseExpectedConstraint := DRRegisterDomain
		                           greaterOrEquals: falseConstraint reg
		                           than: 20.
	self assert: (trueConstraint includes: trueExpectedConstraint).
	self assert: (falseConstraint includes: falseExpectedConstraint)
]

{ #category : #constraints }
DRPathGenerationTest >> testCopyConstantAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: 1 asDRValue.
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints size equals: 1
]

{ #category : #constraints }
DRPathGenerationTest >> testCopyRegisterConstraint [

	| cfg edge constraint |
	cfg := self generateDruidIRFor: #primitiveReturnOne.

	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	constraint := edge constraintFor: 'ReceiverResultReg'.
	self assert: (constraint includes: (DRRegisterDomain equals: constraint reg than: 1) ).
]

{ #category : #constraints }
DRPathGenerationTest >> testCopyRegisterNotAddNewConstraint [

	| cfg block |
	cfg := self setUpCFG: 2.
	cfg b1 copy: (DRPhysicalRegister name: 'Rx').
	cfg generatePaths.

	block := cfg firstBasicBlock.
	self assert: (self anyIncomingEdge: cfg for: block) constraints isEmpty.
	self assert: (self anyOutgoingEdge: cfg for: block) constraints isEmpty.
]

{ #category : #constraints }
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
DRPathGenerationTest >> testGenerateExpectedEdgesCountMerging [

	| cfg edges |
	cfg := self setUpCFGWithConditional.
	cfg newBasicBlockWith: [ :b | cfg b4 jumpTo: b ].
	cfg generatePaths.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
]

{ #category : #constraints }
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
