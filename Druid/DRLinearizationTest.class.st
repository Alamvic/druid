Class {
	#name : #DRLinearizationTest,
	#superclass : #DRPrimitiveCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlow [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4.
	
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b3.
	b2 jumpTo: b4.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
	
	cfg := DRControlFlowGraph new
		initialBasicBlock: b1;
		exitBasicBlock: b4.
		
	self assert: cfg blocks asArray equals: { b1 . b2 . b3 . b4 }
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testBranchingControlFlowWithUnevenBranches [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg b4 |
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	b4 := DRBasicBlock id: 4.
	
	b1
		jumpIfTrueTo: b2
		ifFalseTo: b4.
	b2 jumpTo: b3.
	b3 jumpTo: b4.
	b4 endInstruction: DRReturn new.
	
	cfg := DRControlFlowGraph new
		initialBasicBlock: b1;
		exitBasicBlock: b4.
		
	self assert: cfg blocks asArray equals: { b1 . b2 . b3 . b4 }
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocks [

	| method irGenerator cfg |
	method := interpreter class lookupSelector: #primitiveWithIfAssigningValue.

	irGenerator :=  DRPrimitiveIRGenerator new.
	irGenerator
		numberOfArguments: 0;
		interpretMethod: method ast
		receiver: (DRInterpreterValue value: interpreter).
	cfg := irGenerator ir.
	
	self assert: cfg blocks size equals: 5
]

{ #category : #'tests-building' }
DRLinearizationTest >> testBuildCFGHasAllBlocksWhenEarlyMethodReturns [

	| method irGenerator cfg |
	method := interpreter class lookupSelector: #primitiveCallingMethodWithEarlyReturn.

	irGenerator :=  DRPrimitiveIRGenerator new.
	irGenerator
		numberOfArguments: 0;
		interpretMethod: method ast
		receiver: (DRInterpreterValue value: interpreter).
	cfg := irGenerator ir.
	
	self assert: cfg blocks size equals: 5
]

{ #category : #'tests-basic' }
DRLinearizationTest >> testLinearControlFlow [

	"A block should be visited after all its predecessors are visited"
	
	| b1 b2 b3 cfg |
	b1 := DRBasicBlock id: 1.
	b2 := DRBasicBlock id: 2.
	b3 := DRBasicBlock id: 3.
	
	b1 jumpTo: b2.
	b2 jumpTo: b3.
	b3 endInstruction: DRReturn new.
	
	cfg := DRControlFlowGraph new
		initialBasicBlock: b1;
		exitBasicBlock: b3.
		
	self assert: cfg blocks asArray equals: { b1 . b2 . b3 }
]
