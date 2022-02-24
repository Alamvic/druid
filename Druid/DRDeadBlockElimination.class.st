Class {
	#name : #DRDeadBlockElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #executing }
DRDeadBlockElimination >> applyTo: aDRControlFlowGraph [ 
	
	| changed |
	changed := true.
	[ changed ] whileTrue: [ 
		changed := self step: aDRControlFlowGraph.
	]
]

{ #category : #testing }
DRDeadBlockElimination >> isDead: aDRBasicBlock in: aCFG [
	
	^ aCFG initialBasicBlock ~= aDRBasicBlock and: [ aDRBasicBlock predecessors isEmpty ]
]

{ #category : #executing }
DRDeadBlockElimination >> step: aDRControlFlowGraph [ 
	
	| deadBlocks |
	deadBlocks := aDRControlFlowGraph blocks select: [ :e | 
		self isDead: e in: aDRControlFlowGraph ].
	deadBlocks do: [ :e | 
		aDRControlFlowGraph removeBasicBlock: e.
		e successors do: [ :s | s removePredecessor: e ] ].
	^ deadBlocks notEmpty
]
