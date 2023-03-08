Class {
	#name : #DRPrimitiveCompilerCompilerCollectorTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'compilationUnit'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #running }
DRPrimitiveCompilerCompilerCollectorTest >> setUp [

	super setUp.
	StackInterpreter initializePrimitiveTable.
	interpreterCompiler := (DRInterpreterCompiler
			fromInterpreter: interpreterBuilder
			compilerClass: DRPrimitiveCompilerCompilerCollector).
	compilationUnit := interpreterCompiler generateBuildModel
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitCollectsPrimitives [

	self assert: compilationUnit hasPrimitives
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitGeneratesJIT [

	interpreterCompiler generateIRAndCompileIn: self testingJITCompilerClass.

	self shouldnt: [ self class environment at: self testingJITCompilerClass ] raise: KeyNotFound.
	self assert: ((self class environment at: self testingJITCompilerClass) respondsTo: #initializePrimitiveTable)
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitModelGeneratesJIT [

	interpreterCompiler generateBuildModelAndCompileIn: self testingJITCompilerClass.

	self shouldnt: [ self class environment at: self testingJITCompilerClass ] raise: KeyNotFound.
	self assert: ((self class environment at: self testingJITCompilerClass) respondsTo: #initializePrimitiveTable)
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testFailedPrimitivesCollection [

	| cc |
	cc := interpreterCompiler compilerCompiler.
	cc hasFailedPrimitives
		ifTrue: [ self assert: (cc failedPrimitives allSatisfy: #notEmpty) ]
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testObtainCompilationUnit [

	self assert: interpreterCompiler generateBuildModel notNil
]
