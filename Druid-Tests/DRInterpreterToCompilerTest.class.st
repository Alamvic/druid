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
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testAddPrimitives [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveSubtract #primitiveLessThan #primitiveGreaterThan #primitiveLessOrEqual #primitiveGreaterOrEqual #primitiveEqual #primitiveNotEqual #primitiveMultiply #primitiveDivide).
	
	interpreterToCompiler buildIRAndCompileIn: self jitCompilerClassNameForTest.
	self
		assertCollection: (interpreterToCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterToCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testCompilePrimitiveTable [

	interpreterToCompiler buildIRAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DREmptyInterpreter.
	self assert: interpreterToCompiler primitives isEmpty
]
