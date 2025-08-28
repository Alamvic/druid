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

{ #category : 'accessing' }
DRInsertPiNodes >> insertPiNode: aDRPiNode in: aDRBasicBlock withoutModifyingAnyOf: piNodesNotToModify [

	aDRBasicBlock addInstructionFirst: aDRPiNode.

	aDRPiNode operand users copy
		select: [ :user | (piNodesNotToModify includes: user) not and: [
								(self shouldReplaceUser: user withPiNode: aDRPiNode )] ]
		thenDo: [ :user | user replaceDependency: aDRPiNode operand by: aDRPiNode.].

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
	| piNodeBuilder |

	piNodeBuilder := DRPiNodeBuilder withInstructionFactory: aDRBranchIfCondition basicBlock controlFlowGraph instructionFactory.
	aDRBranchIfCondition condition
		acceptVisitor: piNodeBuilder
		withBranch: aDRBranchIfCondition.

	self insertPiNodes: piNodeBuilder nodesForTrueBranch in: (self
			 getBasicBlockToInsertPiNodesForBranch:
			 aDRBranchIfCondition trueBranch
			 ofCondition: aDRBranchIfCondition).

	self insertPiNodes: piNodeBuilder nodesForFalseBranch in: (self
			 getBasicBlockToInsertPiNodesForBranch:
			 aDRBranchIfCondition falseBranch
			 ofCondition: aDRBranchIfCondition)
]

{ #category : 'accessing' }
DRInsertPiNodes >> shouldReplaceUser: user withPiNode: aDRPiNode [

	| operandIdx |
	(user basicBlock isDominatedBy: aDRPiNode basicBlock) ifTrue: [ ^ true ].

	user isPhiFunction ifFalse: [ ^ false ].

	operandIdx := user operands indexOf: aDRPiNode operand.
	^ (user basicBlock predecessorAt: operandIdx) isDominatedBy:
		  aDRPiNode basicBlock
]
