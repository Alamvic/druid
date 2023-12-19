Class {
	#name : #DRTailDuplicateStagedExpressions,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRTailDuplicateStagedExpressions >> doApplyWithGeneratedPaths: cfg [

	cfg phiFunctions do: [ :phi |
		phi isStaged ifTrue: [ phi basicBlock tailDuplicate ] ]
]
