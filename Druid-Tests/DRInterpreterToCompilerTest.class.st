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
DRInterpreterToCompilerTest >> testGenerateAlwaysMaycallbackPrimitivesCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter:
		                       (self interpreterWith: #( #primitiveAdd )).
		
	self assert: interpreterCompiler primitives size equals: 1 + 7.
	self
		assert: interpreterCompiler primitives first sourceSelector
		equals: #primitiveAdd.
	self assert:
		(interpreterCompiler primitives allButFirst allSatisfy: [ :primUnit |
			 primUnit mayCallback ])
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives size equals: 7.
	self assert:
		(interpreterCompiler primitives allButFirst allSatisfy: [ :primUnit |
			 primUnit mayCallback ])
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	interpreterCompiler := DRInterpreterCompiler fromInterpreter:
		                       (self interpreterWith:
			                        #( #primitiveAdd #primitiveEqual )).

	self assert: interpreterCompiler primitives size equals: 2 + 7.
	self
		assert: interpreterCompiler primitives first sourceSelector
		equals: #primitiveAdd.
	self
		assert: interpreterCompiler primitives second sourceSelector
		equals: #primitiveEqual.
	self assert:
		((interpreterCompiler primitives allButFirst: 2) allSatisfy: [ :primUnit |
			 primUnit mayCallback ])
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
