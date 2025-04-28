Class {
	#name : 'DRInsertPiNodes',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRInsertPiNodes >> applyTo: cfg [

	cfg blocks do: [ :b |
		b endInstruction isConditionalBranch ifTrue: [
			self insertPiNodesFor: b endInstruction.
		]
	]
]

{ #category : 'enumerating' }
DRInsertPiNodes >> getBasicBlockToInsertPiNodesForBranch: currentBranchBasicBlock ofCondition: aDRBranchIfCondition andDo: aBlock [ 
	
	| newBlock |
	"We need to break critical edges, because in that case, the constraint may not be true in every possible path"
	
	(currentBranchBasicBlock predecessors size == 1) ifTrue: [ ^ aBlock value: currentBranchBasicBlock ].
	
	newBlock := currentBranchBasicBlock breakCriticalEdgeToPredecessor: aDRBranchIfCondition basicBlock.
	
	newBlock controlFlowGraph invalidateDominatorTree; invalidatePostDominatorTree.
	
	^ aBlock value: newBlock
]

{ #category : 'accessing' }
DRInsertPiNodes >> insertPiNode: aDRPiNode in: aDRBasicBlock withoutModifyingAnyOf: aCollection [

	aDRPiNode result: aDRBasicBlock controlFlowGraph instructionFactory allocateTemporaryRegister.
	aDRBasicBlock addInstructionFirst: aDRPiNode.

	aDRPiNode operand users
		select: [ :user | user ~= aDRPiNode and: [
								(aCollection includes: user) not and: [
								(user basicBlock isDominatedBy: aDRBasicBlock) ] ] ]
		thenDo: [ :user | user replaceDependency: aDRPiNode operand by: aDRPiNode ]
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
	
	piNodeBuilder := DRPiNodeBuilder new.
	aDRBranchIfCondition condition
		acceptVisitor: piNodeBuilder
		withBranch: aDRBranchIfCondition.

	self getBasicBlockToInsertPiNodesForBranch: aDRBranchIfCondition trueBranch ofCondition: aDRBranchIfCondition andDo: [ :block |
		self insertPiNodes: piNodeBuilder nodesForTrueBranch in: aDRBranchIfCondition trueBranch
	].

	self getBasicBlockToInsertPiNodesForBranch: aDRBranchIfCondition falseBranch ofCondition: aDRBranchIfCondition andDo: [ :block |
		self insertPiNodes: piNodeBuilder nodesForFalseBranch in: aDRBranchIfCondition falseBranch
	].

]
