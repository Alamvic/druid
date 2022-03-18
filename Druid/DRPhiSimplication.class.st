Class {
	#name : #DRPhiSimplication,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRPhiSimplication >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph phiFunctions do: [ :i | 
		| operands |
		operands := i operands asSet select: [ :o | o isNullValue not ].
		operands isEmpty ifTrue: [ i removeFromCFG ].
		operands size = 1 ifTrue: [ 
			i replaceBy: (DRCopy operands: { i operand1 } result: i result) ] ]
]
