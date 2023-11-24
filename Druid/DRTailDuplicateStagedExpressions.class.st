Class {
	#name : #DRTailDuplicateStagedExpressions,
	#superclass : #DRPathOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #accessing }
DRTailDuplicateStagedExpressions >> doApplyWithGeneratedPaths: cfg [

	cfg phiFunctions do: [ :phi |
		(phi operands allSatisfy: [ :op | op isJITCompileTimeExpression ])
			ifTrue: [ phi basicBlock tailDuplicate ] ]
]
