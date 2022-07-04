Class {
	#name : #DRCopyPropagation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRCopyPropagation >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions copy do: [ :i | 
		(i isCopy and: [ 
			 i result isPhysicalRegister not and: [ i operand1 isInstruction ] ]) 
			ifTrue: [ self propagateCopyFrom: i ] ]
]

{ #category : #operations }
DRCopyPropagation >> propagateCopyFrom: i [

	i users do: [ :user | user replaceOperand: i by: i operands first ]
]
