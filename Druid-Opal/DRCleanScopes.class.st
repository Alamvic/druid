Class {
	#name : 'DRCleanScopes',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRCleanScopes >> applyTo: cfg [

	self cleanInstructionsScope: cfg scope for: cfg
]

{ #category : 'as yet unclassified' }
DRCleanScopes >> cleanInstructionsScope: aScope for: cfg [

	| allInstructions anyWrite anyRead |
	allInstructions := cfg allInstructionsWithScope: aScope.

	aScope tempVarNames do: [ :temp |
		anyWrite := allInstructions anySatisfy: [ :i |
			            i isStoreTemporaryVariable and: [
				            i variableName = temp ] ].

		anyRead := allInstructions anySatisfy: [ :i |
			           (i isLoadTemporaryVariable or: [ i isLoadArgument ])
				           and: [ i variableName = temp ] ].

		(anyWrite and: anyRead) ifFalse: [
			allInstructions
				select: [ :i | i variableName = temp ]
				thenDo: [ :i | i hasUsers ifFalse: [ i removeFromCFG ] ] ] ].

	aScope inlinedScopes do: [ :scope |
		self cleanInstructionsScope: scope for: cfg ]
]
