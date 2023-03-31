Class {
	#name : #DRInterpreterBuilderTest,
	#superclass : #DRAbstractMockTest,
	#category : #'Druid-Tests-InterpreterBuilder'
}

{ #category : #running }
DRInterpreterBuilderTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
	interpreterBuilder := DRInterpreterBuilder new
		targetSuperclass: DRBasicCogInterpreterArithmeticPrimitives;
		yourself
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreterBuilder: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self jitCompilerClassForTest.

	self assert: interpreterCompiler initPrimitiveTable notEmpty
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateEmptyCompilationUnit [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreterBuilder: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveEqual).
	interpreterCompiler := DRInterpreterToCompiler fromInterpreterBuilder: (self interpreterWith: primitiveSet).
	self
		assertCollection: (interpreterCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	interpreterCompiler := DRInterpreterToCompiler fromInterpreterBuilder: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateSinglePrimitiveCompilationUnit [

	| primitives |
	interpreterCompiler := DRInterpreterToCompiler fromInterpreterBuilder: (self interpreterWith: #(#primitiveAdd)).
	primitives := interpreterCompiler primitives.
	self
		assert: primitives notEmpty;
		assert: (primitives anyOne isKindOf: DRPrimitiveObject).
	self
		assertCollection: (primitives collect: #sourceSelector)
		hasSameElements: #(primitiveAdd)
]
