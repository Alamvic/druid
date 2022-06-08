Class {
	#name : #DRDeadBlockElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #executing }
DRDeadBlockElimination >> applyTo: aDRControlFlowGraph [ 
	
	| reachableBlocks deadBlocks worklist marked |
	
	worklist := OrderedCollection new.
	worklist add: aDRControlFlowGraph initialBasicBlock.
	marked := Set new.
	
	[ worklist isEmpty ] whileFalse: [ | visitedBlock |
		visitedBlock := worklist removeFirst.
		(marked includes: visitedBlock)
			ifFalse: [ 
				marked add: visitedBlock.
				worklist addAll: visitedBlock successors ] ].
	
	"Now remove all not available in a post order traversal"
	reachableBlocks := marked.
	deadBlocks := aDRControlFlowGraph blocks reject: [ :e | 
		reachableBlocks includes: e ].
	deadBlocks do: [ :e | 
		aDRControlFlowGraph removeBasicBlock: e.
		e successors do: [ :s | s removePredecessor: e ] ].
]

{ #category : #executing }
DRDeadBlockElimination >> step: aDRControlFlowGraph [ 
	
	| deadBlocks reachableBlocks |
	"First remove dead blocks connected as predecessors.
	Otherwise they prevent correct post order traversal"
	deadBlocks := aDRControlFlowGraph blocks select: [ :e | 
		e ~= aDRControlFlowGraph initialBasicBlock
			and: [ e predecessors isEmpty] ].
	deadBlocks do: [ :e | 
		aDRControlFlowGraph removeBasicBlock: e.
		e successors do: [ :s | s removePredecessor: e ] ].
	^ deadBlocks notEmpty
]
