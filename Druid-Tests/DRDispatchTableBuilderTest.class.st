"
A DRCogitPrimitiveDumperTest is a test class for testing the behavior of DRCogitPrimitiveDumper
"
Class {
	#name : #DRDispatchTableBuilderTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'dispatchTableBuilder'
	],
	#category : #'Druid-Tests-InterpreterBuilder'
}

{ #category : #running }
DRDispatchTableBuilderTest >> compilationUnitForTesting [

	^ DRCogitCompilationUnit new
		targetClass: self jitCompilerClassForTest;
		yourself
]

{ #category : #running }
DRDispatchTableBuilderTest >> setUp [

	super setUp.
	interpreter :=	DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.
	dispatchTableBuilder := DRCogitDispatchTableBuilder fromCompilationUnit: self compilationUnitForTesting
]

{ #category : #tests }
DRDispatchTableBuilderTest >> testCollectAllMethodsOn [

	| pcgMethodNodes |
	
	self assertEmpty: dispatchTableBuilder initPrimitiveTable.
	self assertEmpty: dispatchTableBuilder initBytecodeTable.
	
	pcgMethodNodes := dispatchTableBuilder collectAllMethodsOn: self jitCompilerClassForTest class.
	self
		assertCollection: (pcgMethodNodes collect: #selector)
		hasSameElements: #(primitiveTableArray initializePrimitiveTable initializeBytecodeTableForSistaV1)
]
