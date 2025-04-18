Class {
	#name : 'DRPiNodeBuilder',
	#superclass : 'Object',
	#instVars : [
		'trueNodes'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'private - building' }
DRPiNodeBuilder >> createPiNode: aDRConditionalConstraintClass on: aValue withTheOtherOperand: otherOperands then: aBlock [ 
	
	otherOperands detect: [ :otherOp | otherOp ~= aValue ] ifFound: [ :otherOp |
		aBlock value: (DRPiNode
			constraint: (aDRConditionalConstraintClass withValue: otherOp)
			on: aValue)
	]
]

{ #category : 'initialization' }
DRPiNodeBuilder >> initialize [ 
	
	trueNodes := OrderedCollection new.
]

{ #category : 'accesing' }
DRPiNodeBuilder >> nodesForFalseBranch [
	
	^ trueNodes collect: [ :pi | pi negated ]

]

{ #category : 'accesing' }
DRPiNodeBuilder >> nodesForTrueBranch [
	
	^ trueNodes
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitComparison: aDRBranchIfCondition withConstraint: constraint [

	aDRBranchIfCondition operands
		select: [ :operand | operand isInstruction or: operand isRegister ]
		thenDo: [ :operand |
			self
				createPiNode: constraint
				on: operand
				withTheOtherOperand: aDRBranchIfCondition operands
				then: [ :pi | trueNodes add: pi ] ]
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitEqualsThan: aDRCondition inBranch: aDRBranchIfCondition [

	self visitComparison: aDRBranchIfCondition withConstraint: DREqualsConstraint
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitGreaterOrEqualsThan: aDRCondition inBranch: aDRBranchIfCondition [ 

	self visitComparison: aDRBranchIfCondition withConstraint: DRGreaterOrEqualsConstraint.
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitLessOrEqualsThan: aDRCondition inBranch: aDRBranchIfCondition [ 

	self visitComparison: aDRBranchIfCondition withConstraint: DRLessOrEqualsConstraint.
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitLessThan: aDRCondition inBranch: aDRBranchIfCondition [ 

	self visitComparison: aDRBranchIfCondition withConstraint: DRLessConstraint.
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitNoTestMask: aDRCondition inBranch: aDRBranchIfCondition [ 

	self visitComparison: aDRBranchIfCondition withConstraint: DRNoneMaskConstraint.
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitTestMask: aDRCondition inBranch: aDRBranchIfCondition [ 

	self visitComparison: aDRBranchIfCondition withConstraint: DRAnyMaskConstraint.
]
