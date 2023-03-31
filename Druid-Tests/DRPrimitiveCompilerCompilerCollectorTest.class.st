Class {
	#name : #DRPrimitiveCompilerCompilerCollectorTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'compilationUnit'
	],
	#category : #'Druid-Tests-InterpreterBuilder'
}

{ #category : #running }
DRPrimitiveCompilerCompilerCollectorTest >> setUp [

	super setUp.
	StackInterpreter initializePrimitiveTable.
	interpreterCompiler := (DRInterpreterToCompiler
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

	interpreterCompiler generateIRAndCompileIn: self jitCompilerClassForTest.

	self shouldnt: [ self class environment at: self jitCompilerClassForTest ] raise: KeyNotFound.
	self assert: ((self class environment at: self jitCompilerClassForTest) respondsTo: #initializePrimitiveTable)
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitModelGeneratesJIT [

	interpreterCompiler generateBuildModelAndCompileIn: self jitCompilerClassForTest.

	self shouldnt: [ self class environment at: self jitCompilerClassForTest ] raise: KeyNotFound.
	self assert: ((self class environment at: self jitCompilerClassForTest) respondsTo: #initializePrimitiveTable)
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
