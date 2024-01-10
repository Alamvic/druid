Class {
	#name : #DRTailDuplicateStagedExpressions,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRTailDuplicateStagedExpressions >> doApplyWithGeneratedPaths: cfg [

	| jitCompileConditionalBlocks |
	jitCompileConditionalBlocks := cfg allConditionalJumps
		                               select: [ :jump |
		                               jump isJITCompileTimeExpression ]
		                               thenCollect: [ :jump |
		                               jump basicBlock ].

	cfg phiFunctions
		select: [ :phi |
			jitCompileConditionalBlocks anySatisfy: [ :dominator |
				phi basicBlock isDominatedBy: dominator ] ]
		thenDo: [ :phi |
			phi isJITCompileTimeExpression ifTrue: [
				phi basicBlock tailDuplicate ] ]
]
