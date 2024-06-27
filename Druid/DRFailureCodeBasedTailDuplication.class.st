Class {
	#name : #DRFailureCodeBasedTailDuplication,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRFailureCodeBasedTailDuplication >> applyTo: aDRControlFlowGraph [

	aDRControlFlowGraph exitBasicBlocks do: [ :bb |
		(bb predecessors size > 1 and: [
			 bb instructions noneSatisfy: [ :e | e isStackEffect ] ]) ifTrue: [
			bb tailDuplicate ] ]
]

{ #category : #testing }
DRFailureCodeBasedTailDuplication >> canBeAppliedIn: aDRControlFlowGraph [

	^ aDRControlFlowGraph exitBasicBlocks anySatisfy: [ :bb |
		  bb predecessors size > 1 ]
]
