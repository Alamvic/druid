Class {
	#name : 'DRStoreTemporaryStagedLoop',
	#superclass : 'DROptimisation',
	#category : 'Druid-BytecodeToJITCompilation',
	#package : 'Druid',
	#tag : 'BytecodeToJITCompilation'
}

{ #category : 'accessing' }
DRStoreTemporaryStagedLoop >> applyTo: cfg [

	cfg phiFunctions
		select: [ :phi | phi hasRecursiveUse and: [ phi isJITCompileTimeExpression ] ]
		thenDo: [ :phi | self storeOnNewTemporaryVariables: phi ]
]

{ #category : 'store' }
DRStoreTemporaryStagedLoop >> storeOnNewTemporaryVariables: phiFunction [

	| variableExpression load instructionFactory |
	instructionFactory := phiFunction instructionFactory.
	variableExpression := (DRInterpreterReference reference:
			                       phiFunction result prettyPrint asLowercase)
		                      type: (DRSignedIntegerType size: 8);
		                      yourself.

	"Replace operands by staged stores"
	phiFunction operands do: [ :op |
		| store |
		store := instructionFactory storeSInt64: op at: variableExpression.
		op addNext: store.
		phiFunction replaceOperand: op by: store ].

	"Replace self by staged load"
	load := instructionFactory loadSInt64: variableExpression.
	load result: phiFunction result.
	phiFunction addNext: load.
	phiFunction replaceUsesBy: load.

	"Keep the recursive phi"
	load addOperand: phiFunction.
	self assert: phiFunction hasRecursiveUse
]
