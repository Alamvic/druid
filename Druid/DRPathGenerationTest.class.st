Class {
	#name : #DRPathGenerationTest,
	#superclass : #DRIRTest,
	#category : #'Druid-Tests'
}

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromInnerBranches [

	| cfg edges trueConstraint falseConstraint innerMergeBlock |
	cfg := self generateDruidIRFor: #primitiveInnerBranching.

	innerMergeBlock := cfg mergeBlocks first.
	edges := cfg incomingEdgesFor: innerMergeBlock.
	trueConstraint := edges first constraintFor: 'ReceiverResultReg'. "10 < Reg < 20"
	falseConstraint := edges second constraintFor: 'ReceiverResultReg'. ">= 20"

	self assertConstraint: trueConstraint includes: (DRIntersectionConstraint between: 10 and: 20).
	self assertConstraint: falseConstraint includes: (DRGreaterOrEqualsConstraint withValue: 20)
]

{ #category : #constraints }
DRPathGenerationTest >> testConstraintsInferenceFromLoop [

	| cfg edges firstPhiConstraint loopedPhiConstraint sumConstraint returnInstruction |
	cfg := self generateDruidIRFor: #primitiveLoopIncrementing.

	"Go out from loop"
	returnInstruction := cfg instructions detect: [ :i | i isReturn ].
	edges := cfg incomingEdgesFor: returnInstruction basicBlock.

	"If never go inside the loop"
	firstPhiConstraint := edges first constraintFor: 'R6'.
	"If enter and then go out from loop"
	loopedPhiConstraint := edges second constraintFor: 'R6'.
	sumConstraint := edges second constraintFor: 'R11'.

	self assert: firstPhiConstraint isEmpty.
	self assertConstraint: loopedPhiConstraint includes: (DRGreaterOrEqualsConstraint withValue: 15).
	self assert: sumConstraint isNotNil
]

{ #category : #'dead-path' }
DRPathGenerationTest >> testInferenceDeadPaths [

	| cfg deadPath expectedTarget deadPathBlocks deadCopy expectedPath |
	cfg := self generateDruidIRFor: #primitiveInnerBranchingWithDeadBranch.

	deadPath := cfg deadPaths associations first.
	deadPathBlocks := deadPath value collect: [ :e | e destination ].
	
	expectedTarget := self anyOutgoingEdge: cfg for: (cfg firstBasicBlock).
	deadCopy := cfg instructions detect: [:i | i isCopy and: [ i operand1  value = 123 ] ].
	expectedPath := {deadCopy basicBlock . deadCopy basicBlock successor . deadCopy basicBlock successor successor }.

	self assert: deadPath key equals: expectedTarget.
	self assertCollection: deadPathBlocks hasSameElements: expectedPath
]
