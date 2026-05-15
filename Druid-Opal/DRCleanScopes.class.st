Class {
	#name : 'DRCleanScopes',
	#superclass : 'DROptimisation',
	#category : 'Druid-Opal-Optimizations',
	#package : 'Druid-Opal',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRCleanScopes >> applyTo: cfg [

	self cleanScope: cfg scope for: cfg
]

{ #category : 'as yet unclassified' }
DRCleanScopes >> cleanInit: aScope for: cfg [

	| scopedInstructions |
	"Avoid links from inner to outer scopes"
	aScope inlinedScopes ifNotEmpty: [ ^ self ].
	"Method must have a scope?"
	cfg scope = aScope ifTrue: [ ^ self ].

	scopedInstructions := cfg allInstructionsWithScope: aScope.
	scopedInstructions isEmpty ifTrue: [
		aScope initInstruction ifNotNil: [ :i | i removeFromCFG ] ]
]

{ #category : 'as yet unclassified' }
DRCleanScopes >> cleanScope: aScope for: cfg [

	| allInstructions |
	allInstructions := cfg allInstructionsWithScope: aScope.

	"After optimizations, some temporaries maybe are not used"
	aScope tempVarNames do: [ :temp |
		self cleanTemporary: temp from: aScope withInstructions: allInstructions ].

	"If all temporaries were optimized, then the scope is not needed"
	self cleanInit: aScope for: cfg.

	"Continue deeper..."
	aScope inlinedScopes do: [ :scope |
		self cleanScope: scope for: cfg ]
]

{ #category : 'as yet unclassified' }
DRCleanScopes >> cleanTemporary: temp from: aDRScope withInstructions: allInstructions [

	| variableInstructions |
	variableInstructions := allInstructions select: [ :i | i variableName = temp ].
	(self isUsedVariable: variableInstructions) ifFalse: [
			variableInstructions do: [ :i |
				i hasUsers ifFalse: [ i removeFromCFG ] ] ]
]

{ #category : 'asserting' }
DRCleanScopes >> isUsedVariable: variableInstructions [

	| anyWrite anyRead |
	anyWrite := variableInstructions anySatisfy: [ :i |
		            i isStoreTemporaryVariable ].
	anyRead := variableInstructions anySatisfy: [ :i |
		           i isLoadTemporaryVariable or: [ i isLoadArgument ] ].
	
	^ anyWrite and: anyRead
]
