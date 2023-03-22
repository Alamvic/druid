Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractMockTest,
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #initialization }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self jitCompilerClassForTest.

	self assert: interpreterCompiler initPrimitiveTable notEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveEqual).
	interpreterCompiler := DRInterpreterToCompiler fromInterpreter: (self interpreterWith: primitiveSet).
	self
		assertCollection: (interpreterCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	| primitives |
	interpreterCompiler := DRInterpreterToCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	primitives := interpreterCompiler primitives.
	self
		assert: primitives notEmpty;
		assert: (primitives anyOne isKindOf: DRPrimitiveObject).
	self
		assertCollection: (primitives collect: #sourceSelector)
		hasSameElements: #(primitiveAdd)
]
