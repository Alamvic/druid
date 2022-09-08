Class {
	#name : #DRCopyPropagation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRCopyPropagation >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions copy do: [ :i | 
		(i isCopy and: [ i result isPhysicalRegister not and: [ i operand1 isConstant not ] ]) 
			ifTrue: [ self propagateCopyFrom: i ] ]
]

{ #category : #operations }
DRCopyPropagation >> propagateCopyFrom: i [

	i users copy do: [ :user | user replaceOperand: i by: i operands first ]
]
