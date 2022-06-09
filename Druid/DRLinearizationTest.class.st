Class {
	#name : #DRLinearizationTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlow [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	cfg := DRControlFlowGraph new.
	
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	
	cfg initialBasicBlock jumpTo: b1.
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
		
	self assert: cfg reversePostOrderBlocks asArray equals: { cfg initialBasicBlock. b1 . b3 . b2 . b4 }
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowHasCorrectStructure [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	cfg initialBasicBlock jumpTo: b1.	
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
	
	
	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowWithUnevenBranches [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	cfg := DRControlFlowGraph new.
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
		
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b4.
	b2 jumpTo: b3.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
	
	self assert: cfg reversePostOrderBlocks asArray equals: { b1 . b2 . b3 . b4 }
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowWithUnevenBranchesHasCorrectStructure [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	cfg := DRControlFlowGraph new.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.
	
	cfg initialBasicBlock jumpTo: b1.
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b4.
	b2 jumpTo: b3.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
		
	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocks [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveWithIfAssigningValue;
		interpreter: interpreter;
		generateDruidIR.
	
	self assert: cfg reversePostOrderBlocks size equals: 10
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWhenEarlyMethodReturns [

	| cfg |

	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveCallingMethodWithEarlyReturn;
		interpreter: interpreter;
		generateDruidIR.
	
	self assert: cfg reversePostOrderBlocks size equals: 12
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWithTwoNonLocalReturns [

	| cfg |
	
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveCallingMethodWithTwoEarlyReturns;
		interpreter: interpreter;
		generateDruidIR.
	
	DRDeadBlockElimination new applyTo: cfg.
	
	self assert: cfg reversePostOrderBlocks size equals: 12
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasCorrectStructure [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveWithIfAssigningValue;
		interpreter: interpreter;
		generateDruidIR.

	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasCorrectStructureWhenEarlyMethodReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveCallingMethodWithEarlyReturn;
		interpreter: interpreter;
		generateDruidIR.
	
	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testLinearControlFlow [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg |
	cfg := DRControlFlowGraph new.
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	
	b1 jumpTo: b2.
	b2 jumpTo: b3.
	b3 endInstruction: DRReturn new.
	
	self assert: cfg reversePostOrderBlocks asArray equals: { b1 . b2 . b3 }
]
