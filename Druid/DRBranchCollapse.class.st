Class {
	#name : #DRBranchCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRBranchCollapse >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph blocksDo: [ :b | 
		(b endInstruction isBranchIfTrue and: [ 
			b endInstruction operand1 isSetConditionCode ]) ifTrue: [ 
				| replacementBranch |
				replacementBranch := DRBranchIfCondition operands: {
					b endInstruction operand1 condition.
					b endInstruction operand1 operand1.
					b endInstruction operand1 operand2.
				}.
				replacementBranch trueBranch: b endInstruction trueBranch.
				replacementBranch falseBranch: b endInstruction falseBranch.
				b endInstruction replaceBy: replacementBranch ] ]
]
