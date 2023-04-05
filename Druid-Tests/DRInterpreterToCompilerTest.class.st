Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractCompilerCollectorTest,
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #initialization }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self testingJITCompilerClass.

	self assert: interpreterCompiler initPrimitiveTable notEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	self
		assertCollection: (interpreterCompiler primitives collect: #sourceSelector)
		hasSameElements: #(#primitiveAdd #primitiveEqual).

	self
		assert: (interpreterCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	self assert: interpreterCompiler primitives notEmpty
]
