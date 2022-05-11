Class {
	#name : #DRPhiSimplication,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRPhiSimplication >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph phiFunctions do: [ :i | 
		i operands asSet size = 1 ifTrue: [ 
			i replaceBy: (DRCopy operands: { i operand1 } result: i result) ] ]
]
