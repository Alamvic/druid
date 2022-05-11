Class {
	#name : #DRBranchCollapse,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRBranchCollapse >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph blocksDo: [ :b | 
		(b endInstruction isConditionalBranch and: [ 
			 b endInstruction condition users size = 1 ]) ifTrue: [ 
			b endInstruction condition result: DRNoRegister new ] ]
]
