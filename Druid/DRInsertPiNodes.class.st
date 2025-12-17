Class {
	#name : 'DRInsertPiNodes',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRInsertPiNodes >> addCustomization: constraint forInstruction: i [

	| basicBlock piNode cfg |
	basicBlock := i basicBlock.
	cfg := basicBlock controlFlowGraph.
	piNode := (DRPiNode
		           constraint:
			           (DRPiAnyMaskConstraint withValue:
					            constraint constantValue asDRValue)
		           on: i)
		          result: cfg instructionFactory allocateTemporaryRegister;
		          yourself.
	basicBlock addInstruction: piNode.
	piNode operand users copy
		reject: [ :u | u = piNode ]
		thenDo: [ :user |
		user replaceOperandIfDominated: piNode operand by: piNode ]
]

{ #category : 'accessing' }
DRInsertPiNodes >> addPisForCustomizations: cfg [

	| constraint initConstraint |
	cfg initialContrainst ifNil: [ ^ self ].

	initConstraint := cfg initialContrainst.
	"We only support Receiver Constraints for now"
	self assert: (initConstraint isKindOf: DRReceiverConstraint).
	self assert: (initConstraint constraint isKindOf: DREqualsConstraint).
	constraint := initConstraint constraint.

	cfg instructionsDo: [ :i |
		i isLoadReceiver ifTrue: [
			self addCustomization: constraint forInstruction: i ] ]
]

{ #category : 'accessing' }
DRInsertPiNodes >> applyTo: cfg [

	"We need to break critical edges, because in that case, the inserted constraint may not be true in every possible path"
	DRBreakCriticalEdges applyTo: cfg.

	self addPisForCustomizations: cfg.

	cfg allConditionalJumps do: [ :jump | self insertPiNodesFor: jump ]
]

{ #category : 'enumerating' }
DRInsertPiNodes >> getBasicBlockToInsertPiNodesForBranch: currentBranchBasicBlock [

	currentBranchBasicBlock predecessors size == 1 ifTrue: [
		^ currentBranchBasicBlock ].

	Error signal: 'Critical edge found when inserting PiNodes'
]

{ #category : 'as yet unclassified' }
DRInsertPiNodes >> insertPhiNodesFor: aDRBranchIfCondition withTruePis: truePis andFalsePis: falsePis [

	| cfg mergePoints |
	cfg := aDRBranchIfCondition basicBlock controlFlowGraph.

	mergePoints := aDRBranchIfCondition basicBlock immediatelyDominatedBlocks
		remove: truePis first basicBlock;
		remove: falsePis first basicBlock;
		yourself.
		
	mergePoints size > 1 ifTrue: [ |reversePostOrderBlocks|
		reversePostOrderBlocks := cfg reversePostOrderBlocks.
		mergePoints := mergePoints asOrderedCollection sort: [ :a :b |
			(reversePostOrderBlocks indexOf: a) <= (reversePostOrderBlocks indexOf: b) ].
	].

	truePis with: falsePis do: [ :tPi :fPi |
		| phis originalVariable replacableValues |
		originalVariable := tPi operand.
		phis := mergePoints collect: [ :b | b phiWithVariables: #(  ) ].
		replacableValues := OrderedCollection with: tPi with: fPi.
		phis do: [ :p | |phi|
			phi := p replaceOperandsIfDominated: DRNullValue new withAll: replacableValues.
			replacableValues add: phi.
			originalVariable users copy do: [ :user | (user isDominatedBy: phi) ifTrue: [
					user replaceOperandIfDominated: originalVariable by: phi]
			].
			(phi operands noneSatisfy: #isNullValue) ifFalse: [ self error ]
		]
	]
]

{ #category : 'accessing' }
DRInsertPiNodes >> insertPiNode: aDRPiNode in: aDRBasicBlock withoutModifyingAnyOf: piNodesNotToModify [

	aDRBasicBlock addInstructionFirst: aDRPiNode.

	aDRPiNode operand users copy
		reject: [ :user | piNodesNotToModify includes: user ]
		thenDo: [ :user |
			user replaceOperandIfDominated: aDRPiNode operand by: aDRPiNode ]
]

{ #category : 'accessing' }
DRInsertPiNodes >> insertPiNodes: piNodes in: aDRBasicBlock [ 
	
	piNodes do: [ :pi | self
		insertPiNode: pi
		in: aDRBasicBlock
		withoutModifyingAnyOf: piNodes "Don't want the pi nodes I'm adding to have dependencies between them"
	].
]

{ #category : 'accessing' }
DRInsertPiNodes >> insertPiNodesFor: aDRBranchIfCondition [

	| piNodeBuilder truePis falsePis |
	piNodeBuilder := self piNodeBuilderFor: aDRBranchIfCondition.

	truePis := piNodeBuilder nodesForTrueBranch ifEmpty: [ ^ nil ].
	self insertPiNodes: truePis in: (self
			 getBasicBlockToInsertPiNodesForBranch:
			 aDRBranchIfCondition trueBranch).

	falsePis := piNodeBuilder nodesForFalseBranch.
	self insertPiNodes: falsePis in: (self
			 getBasicBlockToInsertPiNodesForBranch:
			 aDRBranchIfCondition falseBranch).

	self
		insertPhiNodesFor: aDRBranchIfCondition
		withTruePis: truePis
		andFalsePis: falsePis
]

{ #category : 'accessing' }
DRInsertPiNodes >> piNodeBuilderFor: aDRBranchIfCondition [

	| piNodeBuilder |
	piNodeBuilder := DRPiNodeBuilder withInstructionFactory:
		                 aDRBranchIfCondition basicBlock controlFlowGraph
			                 instructionFactory.
	aDRBranchIfCondition condition
		acceptVisitor: piNodeBuilder
		withBranch: aDRBranchIfCondition.

	^ piNodeBuilder
]
