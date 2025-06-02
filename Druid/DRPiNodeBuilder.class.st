Class {
	#name : 'DRPiNodeBuilder',
	#superclass : 'Object',
	#instVars : [
		'trueNodes',
		'instructionFactory'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'instance creation' }
DRPiNodeBuilder class >> withInstructionFactory: aDRInstructionFactory [ 
	^ self new withInstructionFactory: aDRInstructionFactory.
]

{ #category : 'initialization' }
DRPiNodeBuilder >> initialize [ 
	
	trueNodes := OrderedCollection new.
]

{ #category : 'accesing' }
DRPiNodeBuilder >> nodesForFalseBranch [
	
	^ trueNodes collect: [ :pi | pi negated
											result: instructionFactory allocateTemporaryRegister;
											yourself ]

]

{ #category : 'accesing' }
DRPiNodeBuilder >> nodesForTrueBranch [
	
	^ trueNodes
]

{ #category : 'visiting' }
DRPiNodeBuilder >> visitComparison: aDRBranchIfCondition withConstraint: constraint [

	(aDRBranchIfCondition operand1 isInstruction or:
		 aDRBranchIfCondition operand1 isRegister) ifTrue: [
		trueNodes add: ((DRPiNode
				  constraint:
					  (constraint withValue: aDRBranchIfCondition operand2)
				  on: aDRBranchIfCondition operand1)
				 result: instructionFactory allocateTemporaryRegister;
				 yourself) ].

	(aDRBranchIfCondition operand2 isInstruction or:
		 aDRBranchIfCondition operand2 isRegister) ifTrue: [
		trueNodes add: ((DRPiNode
				  constraint:
					  (constraint withValue: aDRBranchIfCondition operand1) reversed
				  on: aDRBranchIfCondition operand2)
				 result: instructionFactory allocateTemporaryRegister;
				 yourself) ]
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
DRPiNodeBuilder >> visitGreaterThan: aDRGreaterThanComparison inBranch: aDRBranchIfCondition [ 
	
	self visitComparison: aDRBranchIfCondition withConstraint: DRGreaterConstraint.
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

{ #category : 'initialization' }
DRPiNodeBuilder >> withInstructionFactory: aDRInstructionFactory [ 
	instructionFactory := aDRInstructionFactory.
]
