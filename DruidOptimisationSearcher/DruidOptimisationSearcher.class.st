Class {
	#name : #DruidOptimisationSearcher,
	#superclass : #Object,
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DruidOptimisationSearcher >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations instructionCount: instructionCount [

	(self
		 bestOptimisationForSelector: aSelector
		 optimisationSequence: optimisations
		 postOptimisationSequence: #(  )
		 instructionCount: instructionCount) ifNotNil: [ :optimisation | 
		^ optimisation ].
	(self
		 bestOptimisationForSelector: aSelector
		 optimisationSequence: optimisations
		 postOptimisationSequence: { DRDeadCodeElimination }
		 instructionCount: instructionCount) ifNotNil: [ :optimisation | 
		^ optimisation ].
	(self
		 bestOptimisationForSelector: aSelector
		 optimisationSequence: optimisations
		 postOptimisationSequence: { 
				 DRSCCP.
				 DRCopyPropagation.
				 DRDeadCodeElimination.
				 DRCleanControlFlow }
		 instructionCount: instructionCount) ifNotNil: [ :optimisation | 
		^ optimisation ].
	
	^ optimisations atRandom
]

{ #category : #searching }
DruidOptimisationSearcher >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	| bestOptimisation bestInstructionCount |
	bestOptimisation := nil.
	bestInstructionCount := instructionCount.
	self optimisationList do: [ :potentialMin | 
		| ir |
		"Get a fresh IR and prepare it with a list of previously applied optimisations"
		ir := self newIRFor: aSelector.
		optimisations do: [ :e | ir applyOptimisation: e new ].
		ir applyOptimisation: potentialMin new.
		postSequence do: [ :e | ir applyOptimisation: e new ].
		ir instructions size < bestInstructionCount ifTrue: [ 
			bestOptimisation := potentialMin.
			bestInstructionCount := instructionCount ] ].

	^ bestOptimisation
]

{ #category : #'ir-building' }
DruidOptimisationSearcher >> newIRFor: aSelector [

	| theMemory interpreter |
	theMemory := Spur64BitCoMemoryManager simulatorClass new.
	DruidTestInterpreter
		initializeWithOptions: { 
				#BytesPerWord.
				8.
				#ObjectMemory.
				theMemory class name.
				#bytecodeTableInitializer.
				#initializeBytecodeTableForSistaV1 }
		objectMemoryClass: theMemory class.
	interpreter := DruidTestInterpreter basicNew
		               objectMemory: theMemory;
		               basicInitialize.

	^ DRPrimitiveCompilerCompiler new
		  primitiveName: aSelector;
		  interpreter: interpreter;
		  generateDruidIR
]

{ #category : #optimisations }
DruidOptimisationSearcher >> optimisationList [

	^ { 
		DRBranchCollapse.
		DRFailureCodeBasedTailDuplication.
		DRSCCP.
		DRDeadBlockElimination.
		DRCopyPropagation.
		DRPhiSimplication.
		DRDeadCodeElimination.
		DRRedundantCopyElimination.
		DRCleanControlFlow.
		"DRDeadPathSplitting."
		DRDeadPathElimination }
]

{ #category : #searching }
DruidOptimisationSearcher >> searchFor: aSelector [

	| optimisations ir numberOfInstructions instructionSizes |
	ir := self newIRFor: aSelector.
	optimisations := OrderedCollection new.
	instructionSizes := OrderedCollection new.
	numberOfInstructions := ir instructions size.
	1 to: 30 do: [ :i | 
		| betterOptimisation |
		instructionSizes add: numberOfInstructions.
		betterOptimisation := self
			                      bestOptimisationForSelector: aSelector
			                      optimisationSequence: optimisations
			                      instructionCount: numberOfInstructions.
		ir applyOptimisation: betterOptimisation new.
		optimisations add: betterOptimisation.
		numberOfInstructions := ir instructions size ].
	1halt.
	^ instructionSizes
]
