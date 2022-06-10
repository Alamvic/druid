Class {
	#name : #DRPathGenerationTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

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
	trueNDomain := edges first constraintFor: 'ReceiverResultReg'.  "n ~= 2"
	trueMDomain := edges first constraintFor: 'Arg0Reg'.  "m < 5"
	firstFalseNDomain := edges second constraintFor: 'ReceiverResultReg'.  "n = 2"
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
	trueRegDomain := edges first constraintFor: 'ReceiverResultReg'.  "n ~= 2 AND n < 5"
	firstFalseRegDomain := edges second constraintFor: 'ReceiverResultReg'.  "n = 2"
	secondFalseRegDomain := edges third constraintFor: 'ReceiverResultReg'.  "n >= 5"

	self assert: (trueRegDomain includes: (DRIntersectionConstraint constraints: {
		DRNotEqualsConstraint withValue: 2.
		DRLessConstraint withValue: 5
	})).
	
	self assert: (firstFalseRegDomain includes: (DREqualsConstraint withValue: 2)).
	self assert: (secondFalseRegDomain includes: (DRGreaterOrEqualsConstraint withValue: 5)).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThan.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. ">= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "< 10"
	
	self assert: trueConstraint includes: (DRGreaterOrEqualsConstraint withValue: 10).
	self assert: falseConstraint includes: (DRLessConstraint withValue: 10).
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromBranchesInverted [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveGreaterOrEqualsThanInverted.

	edges := cfg outgoingEdgesFor: cfg firstBasicBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "<= 10"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. "> 10"
	
	self assert: trueConstraint includes: (DRLessOrEqualsConstraint withValue: 10).
	self assert: falseConstraint includes: (DRGreaterConstraint withValue: 10).
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromCopy [

	| cfg copy edge constraint |
	cfg := self setUpCFG: 2.
	
	copy := cfg b1 copy: 1.	
	cfg generatePaths.

	edge := self anyOutgoingEdge: cfg for: cfg firstBasicBlock.
	constraint := edge constraintFor: copy result name.

	self assert: constraint includes: (DREqualsConstraint withValue: 1).

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
	
	self assert: constraint includes: (DREqualsConstraint withValue: 1).

]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromInnerBranches [

	| cfg edges trueConstraint falseConstraint |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.

	edges := cfg outgoingEdgesFor: (cfg blockById: 30).
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"

	self
		assert: trueConstraint
		includes: (DRIntersectionConstraint between: 10 and: 20).
	self
		assert: falseConstraint
		includes: (DRGreaterOrEqualsConstraint withValue: 20)
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromPhi [

	| cfg edge constraint copy1 copy2 phi |
	cfg := self setUpCFGWithConditional.

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: copy1 with: copy2.
	cfg b4 jumpTo: cfg newBasicBlock.
	cfg generatePaths.

	edge := self anyIncomingEdge: cfg for: cfg lastBasicBlock.
	constraint := edge constraintFor: phi result name.

	self assert: constraint includes: (DRUnionConstraint constraints: { 
				 (DREqualsConstraint withValue: 1).
				 (DREqualsConstraint withValue: 2) })
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromPhiWithNull [

	| cfg edge unionConstraint copy1 copy2 phi |
	cfg := self setUpCFGWithConditional.

	copy1 := cfg b2 copy: 1.
	copy2 := cfg b3 copy: 2.
	phi := cfg b4 phiWith: copy1.
	cfg b4 jumpTo: cfg newBasicBlock.
	cfg generatePaths.

	edge := self anyIncomingEdge: cfg for: cfg lastBasicBlock.
	unionConstraint := edge constraintFor: phi result name.

	self assert: unionConstraint constraints first includes: (DREqualsConstraint withValue: 1).
	self assert: unionConstraint constraints second isAny.

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
	cfg b1 copy: (DRPhysicalRegister name: 'Rx').
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
DRPathGenerationTest >> testGenerateExpectedEdgesCountMerging [

	| cfg edges |
	cfg := self setUpCFGWithConditional.
	cfg newBasicBlockWith: [ :b | cfg b4 jumpTo: b ].
	cfg generatePaths.

	edges := cfg incomingEdgesFor: cfg lastBasicBlock.
	self assert: edges size equals: 2
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
