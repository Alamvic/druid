Class {
	#name : 'DRLinearizationTest',
	#superclass : 'DRDruidTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'tests-basic' }
DRLinearizationTest >> testBranchingControlFlow [
	"A block should be visited after all its predecessors are visited"

	| b1 b2 b3 cfg b4 |
	cfg := DRControlFlowGraph new.

	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	cfg initialBasicBlock jumpTo: b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.

	self assert: cfg reversePostOrderBlocks asArray equals: {
			cfg initialBasicBlock.
			b1.
			b2.
			b3.
			b4 }
]

{ #category : 'tests-basic' }
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

{ #category : 'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowWithStackDependency [
	"A block should be visited after all its predecessors AND stack dependencies are visited"

	| b1 b2 b3 cfg b4 firstPush secondPush |
	cfg := DRControlFlowGraph new.

	"|0| -> |1| -> |2| -> |4|
             \ -> |3| --^
	"
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	cfg initialBasicBlock jumpTo: b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.

	firstPush := b2 push: 1.
	secondPush := b3 push: 2.

	secondPush stackDependencies: {firstPush}.

	self assert: cfg reversePostOrderBlocks asArray equals: {
			b1.
			b2.
			b3.
			b4 }.
	self deny: (b3 canBeLinearizedBefore: b2)
]

{ #category : 'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowWithStackDependencyReversed [
	"A block should be visited after all its predecessors AND stack dependencies are visited"

	| b1 b2 b3 cfg b4 firstPush secondPush |
	cfg := DRControlFlowGraph new.

	"|0| -> |1| -> |2| -> |4|
             \ -> |3| --^
	"
	b1 := cfg initialBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	b4 := cfg newBasicBlock.

	cfg initialBasicBlock jumpTo: b1.
	b1 jumpIfTrueTo: b2 ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.

	firstPush := b3 push: 1.
	secondPush := b2 push: 2.

	secondPush stackDependencies: {firstPush}.

	self assert: cfg reversePostOrderBlocks asArray equals: {
			b1.
			b3.
			b2.
			b4 }.
	self deny: (b2 canBeLinearizedBefore: b3)
]

{ #category : 'tests-basic' }
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

{ #category : 'tests-basic' }
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

{ #category : 'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocks [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveIfTrueIfFalseAssigningValue;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	self assert: cfg reversePostOrderBlocks size equals: 7 + 1.
	self assert: cfg instructions size equals: 22
]

{ #category : 'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWhenEarlyMethodReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveCallingMethodWithEarlyReturn;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	self assert: cfg reversePostOrderBlocks size equals: 8.
	self assert: cfg instructions size equals: 22
]

{ #category : 'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWithTwoNonLocalReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		       primitiveName: #primitiveCallingMethodWithTwoEarlyReturns;
		       interpreter: interpreter;
		       generateDruidIR.

	cfg applyOptimisation: DRCleanControlFlow new.

	DRDeadBlockElimination new applyTo: cfg.

	self assert: cfg reversePostOrderBlocks size equals: 8.
	self assert: cfg instructions size equals: 22
]

{ #category : 'tests-building' }
DRLinearizationTest >> testBuildCFGHasCorrectStructure [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveIfTrueIfFalseAssigningValue;
		interpreter: interpreter;
		generateDruidIR.

	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : 'tests-building' }
DRLinearizationTest >> testBuildCFGHasCorrectStructureWhenEarlyMethodReturns [

	| cfg |
	cfg := DRPrimitiveCompilerCompiler new
		primitiveName: #primitiveCallingMethodWithEarlyReturn;
		interpreter: interpreter;
		generateDruidIR.
	
	"Smoke test. Should fail with an exception if the basic block structure is wrong (successors do not match predecessors)"
	cfg validate
]

{ #category : 'tests-basic' }
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

{ #category : 'tests-basic' }
DRLinearizationTest >> testLoopControlFlow [

	"A block should be visited after all its predecessors are visited"
	
	| b0 b1 b2 b3 cfg |
	cfg := DRControlFlowGraph new.
	b0 := cfg initialBasicBlock.
	b1 := cfg newBasicBlock.
	b2 := cfg newBasicBlock.
	b3 := cfg newBasicBlock.
	
	b0 jumpIfTrueTo: b1 ifFalseTo: b2.
	b1 backJumpTo: b0. 
	b2 jumpTo: b3.
	b3 endInstruction: DRReturn new.
	
	self assert: cfg reversePostOrderBlocks asArray equals: { b0 . b1 . b2 . b3 }
]
