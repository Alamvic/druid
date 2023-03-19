"
A DRCogitPrimitiveDumperTest is a test class for testing the behavior of DRCogitPrimitiveDumper
"
Class {
	#name : #DRDispatchTableBuilderTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'dispatchTableBuilder'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #running }
DRDispatchTableBuilderTest >> compilationUnitForTesting [

	^ DRCompilationUnit new
		targetClass: self testingJITCompilerClass;
		yourself.
]

{ #category : #running }
DRDispatchTableBuilderTest >> setUp [

	super setUp.
	interpreter :=	DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	dispatchTableBuilder := DRCogitDispatchTableBuilder primitiveTableMethodFrom: self compilationUnitForTesting
]

{ #category : #tests }
DRDispatchTableBuilderTest >> testCollectAllMethodsOn [

	| pcgMethodNodes |
	self assert: dispatchTableBuilder initPrimitiveTable isEmpty.
	pcgMethodNodes := dispatchTableBuilder collectAllMethodsOn: self testingJITCompilerClass class.
	self
		assertCollection: (pcgMethodNodes collect: #selector)
		hasSameElements: #(primitiveTableArray initializePrimitiveTable)
]

{ #category : #initialization }
DRDispatchTableBuilderTest >> testingJITCompilerClass [

	^ interpreter environmentAt: super testingJITCompilerClass
]
