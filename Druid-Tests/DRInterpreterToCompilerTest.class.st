Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'interpreterToCompiler'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #running }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals.
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterToCompiler generateIRAndCompileIn: self jitCompilerClassForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DREmptyInterpreter.
	self assert: interpreterToCompiler primitives isEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveSubtract #primitiveLessThan #primitiveGreaterThan #primitiveLessOrEqual #primitiveGreaterOrEqual #primitiveEqual #primitiveNotEqual #primitiveMultiply #primitiveDivide).
	
	interpreterToCompiler generateIRAndCompileIn: self jitCompilerClassForTest.
	self
		assertCollection: (interpreterToCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterToCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]
