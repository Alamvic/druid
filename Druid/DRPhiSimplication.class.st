Class {
	#name : #DRPhiSimplication,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRPhiSimplication >> applyTo: aDRControlFlowGraph [
	
	aDRControlFlowGraph instructions copy do: [ :i |
		(i isPhiFunction and: [ i operands size = 1 ]) ifTrue: [
			i replaceBy: (DRCopy
				operands: { i operand1 }
		      result: i result) ] ]
]
