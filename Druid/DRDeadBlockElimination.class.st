Class {
	#name : #DRDeadBlockElimination,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #executing }
DRDeadBlockElimination >> applyTo: aDRControlFlowGraph [

	| reachableBlocks deadBlocks |
	reachableBlocks := { aDRControlFlowGraph initialBasicBlock }
		                   asOrderedCollection whileNotEmpty: [ :visitedBlock :worklist :marked | 
		                   (marked includes: visitedBlock) ifFalse: [ 
			                   marked add: visitedBlock.
			                   worklist addAll: visitedBlock successors ] ].


	"Now remove all not available in a post order traversal"
	deadBlocks := aDRControlFlowGraph blocks reject: [ :e | 
		              reachableBlocks includes: e ].
	deadBlocks do: [ :e | 
		e successors do: [ :s | s removePredecessor: e ] ].
	deadBlocks do: [ :e | e removeFromCFG ]
]
