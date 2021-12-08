Class {
	#name : #DRCopyPropagation,
	#superclass : #Object,
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRCopyPropagation >> applyTo: aDRControlFlowGraph [
	
	aDRControlFlowGraph instructions copy do: [ :i |
		((i isCopy or: [ i isPhiFunction and: [ i operands size = 1 ] ])
			and: [ i operand1 isInstruction ]) ifTrue: [
				self propagateCopyFrom: i ] ]
]

{ #category : #operations }
DRCopyPropagation >> propagateCopyFrom: i [

	i users copy do: [ :user |
		user replaceOperand: i by: i operands first ]
]
