Class {
	#name : 'DRInsertPiNodes',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRInsertPiNodes >> applyTo: cfg [

	cfg allConditionalJumps do: [ :jump |
		self insertPiNodesFor: jump
	]
]

{ #category : 'enumerating' }
DRInsertPiNodes >> getBasicBlockToInsertPiNodesForBranch: currentBranchBasicBlock ofCondition: aDRBranchIfCondition [

	| newBlock |
	"We need to break critical edges, because in that case, the constraint may not be true in every possible path"
	currentBranchBasicBlock predecessors size == 1 ifTrue: [
		^ currentBranchBasicBlock ].

	newBlock := currentBranchBasicBlock breakCriticalEdgeToPredecessor:
		            aDRBranchIfCondition basicBlock.

	newBlock controlFlowGraph
		invalidateDominatorTree;
		invalidatePostDominatorTree.

	^ newBlock
]

{ #category : 'as yet unclassified' }
DRInsertPiNodes >> insertPhiNodesFor: aDRBranchIfCondition withTruePis: truePis andFalsePis: falsePis [

	| cfg mergePoints |
	cfg := aDRBranchIfCondition basicBlock controlFlowGraph.

	mergePoints := aDRBranchIfCondition basicBlock immediatelyDominatedBlocks
		remove: truePis first basicBlock;
		remove: falsePis first basicBlock;
		yourself.

	truePis with: falsePis do: [ :tPi :fPi |
		| phis originalVariable |
		originalVariable := tPi operand.
		phis := mergePoints collect: [ :b | b phiWithVariables: #(  ) ].
		phis do: [ :p | |phi|
			phi := p replaceOperandsIfDominated: DRNullValue new withAll: { tPi. fPi }.
			originalVariable users copy do: [ :user | (user isDominatedBy: phi) ifTrue: [
					user replaceOperandIfDominated: originalVariable by: phi]
			]
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
			 aDRBranchIfCondition trueBranch
			 ofCondition: aDRBranchIfCondition).

	falsePis := piNodeBuilder nodesForFalseBranch.
	self insertPiNodes: falsePis in: (self
			 getBasicBlockToInsertPiNodesForBranch:
			 aDRBranchIfCondition falseBranch
			 ofCondition: aDRBranchIfCondition).

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
