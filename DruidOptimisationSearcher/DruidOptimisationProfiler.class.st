Class {
	#name : #DruidOptimisationProfiler,
	#superclass : #Object,
	#instVars : [
		'compilerCompiler',
		'interpreter'
	],
	#category : #DruidOptimisationSearcher
}

{ #category : #searching }
DruidOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations instructionCount: instructionCount [

	(self
		 bestOptimisationForSelector: aSelector
		 optimisationSequence: optimisations
		 postOptimisationSequence: #(  )
		 instructionCount: instructionCount) ifNotNil: [ :optimisation | ^ optimisation ].

	^ DRNoOptimisation new
]

{ #category : #searching }
DruidOptimisationProfiler >> bestOptimisationForSelector: aSelector optimisationSequence: optimisations postOptimisationSequence: postSequence instructionCount: instructionCount [

	self subclassResponsibility
]

{ #category : #'building report' }
DruidOptimisationProfiler >> buildFullReport [

	^ String streamContents: [ :stream | 
		  self primitiveList do: [ :primitive | 
			  stream
				  nextPutAll: (self buildReportFor: primitive);
				  cr;
				  cr;
				  cr ] ]
]

{ #category : #'building report' }
DruidOptimisationProfiler >> buildReportFor: selector [

	| result |
	result := self runForSelector: selector.

	^ String streamContents: [ :stream |
		  stream
			  nextPutAll: result first;
			  nextPutAll: '(';
			  nextPutAll: result second value asString;
			  nextPutAll: ')';
			  nextPutAll: ' by ';
			  nextPutAll: self prettyPrint;
			  cr.

		  (result allButLast allButFirst: 2) do: [ :optimisationRunned | 
			  stream
				  nextPutAll: optimisationRunned key prettyPrint;
				  nextPutAll: ',';
				  nextPutAll: optimisationRunned value asString;
				  cr ] ]
]

{ #category : #'ir-building' }
DruidOptimisationProfiler >> instanceInterpreter [

	| theMemory |
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
		               basicInitialize
]

{ #category : #'ir-building' }
DruidOptimisationProfiler >> newIRFor: aSelector [

	interpreter ifNil: [ self instanceInterpreter ].

	^ DRPrimitiveCompilerCompiler new
		  primitiveName: aSelector;
		  interpreter: interpreter;
		  generateDruidIR
]

{ #category : #'ir-building' }
DruidOptimisationProfiler >> newIRManualSelectedOptimizeFor: aSelector [

	| cfg |
	cfg := self newIRFor: aSelector.
	compilerCompiler optimize: cfg.
	^ cfg
]

{ #category : #optimisations }
DruidOptimisationProfiler >> optimisationList [

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
		  DRDeadEdgeSplitting.
		  DRDeadBranchElimination } collect: [ :opt | opt new ]
]

{ #category : #printing }
DruidOptimisationProfiler >> prettyPrint [

	^ self class name
]

{ #category : #primitives }
DruidOptimisationProfiler >> primitiveList [

	^ { #primitiveAdd. #primitiveSubtract. #primitiveMultiply. 
	#primitiveLessThan. #primitiveGreaterThan. 
	#primitiveLessOrEqual. #primitiveGreaterOrEqual. #primitiveEqual. #primitiveNotEqual. 
	  #primitiveDivide. #primitiveMod. 
	  "#primitiveBitAnd. #primitiveBitOr." #primitiveBitXor.
	  #primitiveBitShift. "#primitiveMakePoint." #primitiveFail. 
	#primitiveDiv. #primitiveQuo.
	
	  "#primitiveAt."
	  "#primitiveAtPut."
	  "#primitiveSize."}
]

{ #category : #searching }
DruidOptimisationProfiler >> runForSelector: aSelector [

	| optimisations ir numberOfInstructions betterOptimisation results |
	ir := self newIRFor: aSelector.
	results := OrderedCollection new.
	optimisations := OrderedCollection new.
	numberOfInstructions := ir instructions size.
	results add: aSelector.
	1 to: 40 do: [ :i | 
		results add: betterOptimisation -> numberOfInstructions "ir veryDeepCopy".
		betterOptimisation := self
			                      bestOptimisationForSelector: aSelector
			                      optimisationSequence: optimisations
			                      instructionCount: numberOfInstructions.
		ir applyOptimisation: betterOptimisation.
		optimisations add: betterOptimisation.
		numberOfInstructions := ir instructions size ].
	results add: ir.
	^ results
]
