Class {
	#name : 'DRRedundantCopyElimination',
	#superclass : 'DROptimisation',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRRedundantCopyElimination >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph instructions copy do: [ :i | 
		(i isCopy and: [ self isRedundant: i ]) ifTrue: [ i removeFromCFG ] ]
]

{ #category : 'testing' }
DRRedundantCopyElimination >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph instructions anySatisfy: [ :i | 
		  i result isPhysicalRegister and: [ i operand1 isPhysicalRegister ] ]
]

{ #category : 'testing' }
DRRedundantCopyElimination >> isRedundant: aDRCopy [

	^ (aDRCopy operand1 isPhysicalRegister and:
		   aDRCopy result isPhysicalRegister) and: [ 
		  aDRCopy operand1 name = aDRCopy result name ]
]
