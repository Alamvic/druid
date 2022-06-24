Class {
	#name : #DRControlFlowOptimisation,
	#superclass : #DROptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRControlFlowOptimisation >> applyTo: cfg [

	self doApply: cfg.
	(DRDeadBlockElimination then: DRPhiSimplication) applyTo: cfg.
	"self fixBackJumps: cfg"
]

{ #category : #accessing }
DRControlFlowOptimisation >> doApply: cfg [

	self subclassResponsibility
]

{ #category : #'post-optimise' }
DRControlFlowOptimisation >> fixBackJumps: cfg [

	cfg blocks do: [ :block | 
		block successors do: [ :succ | 
			(block endInstruction isBackJumpTo: succ) ifTrue: [ "If back-jump to block a recursive phi should exist"
				(succ phiFunctions anySatisfy: [ :phi | phi hasRecursiveUse ]) 
					ifFalse: [ "If not, then is a forward-jump" 
						block endInstruction beForwardJumpTo: succ ] ] ].

		"Check if has a back-jump to this block"
		(block phiFunctions anySatisfy: [ :phi | phi hasRecursiveUse ]) 
			ifTrue: [ 
				block backJumpPredecessors do: [ :pred | 
					pred endInstruction beBackJumpTo: block ] ] ]
]
