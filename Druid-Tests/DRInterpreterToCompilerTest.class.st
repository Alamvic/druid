Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractCompilerCollectorTest,
	#category : #'Druid-Tests-InterpreterCompiler'
}

{ #category : #asserting }
DRInterpreterToCompilerTest >> assertCompiledPrimitives: selectedPrimitives [

	selectedPrimitives doWithIndex: [ :selector :index |
		self
			assert: (interpreterCompiler primitives at: index) sourceSelector
			equals: selector ].

	"Assert mandatory primitives compilation (maycallback)"
	self assert:
		((interpreterCompiler primitives allButFirst: selectedPrimitives size)
			 allSatisfy: [ :primUnit |
			 primUnit mayCallback and: [ primUnit supported not ] ])
]

{ #category : #asd }
DRInterpreterToCompilerTest >> initInterpreterCompilerWith: selectedPrimitives [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter:
		                       (self interpreterWith: selectedPrimitives)
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self testingJITCompilerClass.

	self assert: interpreterCompiler initPrimitiveTable notEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	| selectedPrimitives |
	selectedPrimitives := { }.
	self initInterpreterCompilerWith: selectedPrimitives.
	self assertCompiledPrimitives: selectedPrimitives
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| selectedPrimitives |
	selectedPrimitives := { #primitiveAdd . #primitiveEqual }.
	self initInterpreterCompilerWith: selectedPrimitives.
	self assertCompiledPrimitives: selectedPrimitives
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	| selectedPrimitives |
	selectedPrimitives := { #primitiveAdd }.
	self initInterpreterCompilerWith: selectedPrimitives.
	self assertCompiledPrimitives: selectedPrimitives
]
