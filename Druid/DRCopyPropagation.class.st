Class {
	#name : #DRCopyPropagation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRCopyPropagation >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions copy do: [ :i | 
		(i isCopy and: [ 
			 i operand1 isValue not "constant propagation is other optimisation" ]) 
			ifTrue: [ self propagateCopyFrom: i ] ]
]

{ #category : #operations }
DRCopyPropagation >> propagateCopyFrom: i [

	(i users reject: [ :user | user isPhiFunction ]) do: [ :user | 
		user replaceOperand: i by: i operands first ]
]
