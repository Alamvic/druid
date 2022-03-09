Class {
	#name : #DRPathConstraintsTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #constraints }
DRPathConstraintsTest >> testConditionalRegisterConstraint [

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
DRPathConstraintsTest >> testConditionalRegisterConstraintInverted [

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
DRPathConstraintsTest >> testCopyAddNewConstraint [

	| cfg block |
	cfg := self generateDruidIRFor: #primitiveReturnOne.
	self optimize: cfg.
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
DRPathConstraintsTest >> testJumpDoesNotAddConstraint [

	| cfg edge |
	cfg := self setUpCFG: 2.
	self generatePaths: cfg.
	
	edge := cfg outgoingEdgeFor: cfg firstBasicBlock.
	self assert: edge constraints isEmpty.

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
