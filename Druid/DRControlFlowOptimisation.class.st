Class {
	#name : #DRControlFlowOptimisation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRControlFlowOptimisation >> applyTo: cfg [

	self doApply: cfg.
	(DRDeadBlockElimination then: DRPhiSimplication) applyTo: cfg.
	self fixBackJumps: cfg
]

{ #category : #accessing }
DRControlFlowOptimisation >> doApply: cfg [

	self subclassResponsibility
]

{ #category : #'post-optimise' }
DRControlFlowOptimisation >> fixBackJumps: cfg [

	{ cfg initialBasicBlock } asOrderedCollection whileNotEmpty: [ 
		:current 
		:worklist 
		:marked | 
		marked add: current.

		current successors do: [ :succ | 
			(current endInstruction isBackJumpTo: succ)
				ifTrue: [ "Target should be already visited"
					(marked includes: succ) ifFalse: [ "If not, it is not a backjump!!" 
						current endInstruction beForwardJumpTo: succ ] ]
				ifFalse: [ "Successors should not be visited yet"
					(marked includes: succ) ifTrue: [ "Else, it is a backjump!!" 
						current endInstruction beBackJumpTo: succ ] ].

			"Continue with not-visited successors"
			(marked includes: succ) ifFalse: [ worklist add: succ ] ] ].

]
