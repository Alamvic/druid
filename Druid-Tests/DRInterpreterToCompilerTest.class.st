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
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterToCompiler generateIRAndCompileIn: self jitCompilerClassForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]
