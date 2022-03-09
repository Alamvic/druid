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
]
