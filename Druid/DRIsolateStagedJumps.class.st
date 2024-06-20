Class {
	#name : #DRIsolateStagedJumps,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRIsolateStagedJumps >> doApply: cfg [

	| jitCompileConditionalBlocks |
	jitCompileConditionalBlocks := cfg allConditionalJumps
		                               select: [ :jump | jump isStaged ]
		                               thenCollect: [ :jump | jump basicBlock ].
	cfg phiFunctions
		select: [ :phi |
			jitCompileConditionalBlocks anySatisfy: [ :dominator |
				phi basicBlock isDominatedBy: dominator ] ]
		thenDo: [ :phi |
		phi isJITCompileTimeExpression ifTrue: [ phi basicBlock tailDuplicate ] ]
]
