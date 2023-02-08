Class {
	#name : #DRLinearizationTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocks [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveIfTrueIfFalseAssigningValue;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	self assert: cfg reversePostOrderBlocks size equals: 6 + 1.
	self assert: cfg instructions size equals: 19
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWhenEarlyMethodReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveCallingMethodWithEarlyReturn;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	self assert: cfg reversePostOrderBlocks size equals: 7.
	self assert: cfg instructions size equals: 19
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWithTwoNonLocalReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveCallingMethodWithTwoEarlyReturns;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	DRDeadBlockElimination new applyTo: cfg.

	self assert: cfg reversePostOrderBlocks size equals: 7.
	self assert: cfg instructions size equals: 21
]
