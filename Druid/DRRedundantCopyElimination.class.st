Class {
	#name : #DRRedundantCopyElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRRedundantCopyElimination >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions copy do: [ :i | 
		(i isCopy and: [ self isRedundant: i ]) ifTrue: [ i removeFromCFG ] ]
]

{ #category : #testing }
DRRedundantCopyElimination >> isRedundant: aDRCopy [

	^ (aDRCopy operand1 isPhysicalRegister and:
		   aDRCopy result isPhysicalRegister) and: [ 
		  aDRCopy operand1 name = aDRCopy result name ]
]
