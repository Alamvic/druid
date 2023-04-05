"
A DRCogitPrimitiveDumperTest is a test class for testing the behavior of DRCogitPrimitiveDumper
"
Class {
	#name : #DRCogitPrimitiveInstallerTest,
	#superclass : #DRAbstractCompilerCollectorTest,
	#instVars : [
		'primitiveMethodInstaller'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #running }
DRCogitPrimitiveInstallerTest >> compilationUnitForTesting [

	^ DRCompilationUnit new
		jitCompilerClass: self testingJITCompilerClass;
		yourself.
]

{ #category : #running }
DRCogitPrimitiveInstallerTest >> setUp [

	super setUp.
	interpreter :=	DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	primitiveMethodInstaller := DRCogitPrimitiveInstaller primitiveTableMethodFrom: self compilationUnitForTesting.

]

{ #category : #running }
DRCogitPrimitiveInstallerTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass name
]

{ #category : #tests }
DRCogitPrimitiveInstallerTest >> testCollectAllMethodsOn [

	| pcgMethodNodes |
	self assert: primitiveMethodInstaller initPrimitiveTable isEmpty.
	pcgMethodNodes := primitiveMethodInstaller collectAllMethodsOn: self testingJITCompilerClass class.
	self
		assertCollection: (pcgMethodNodes collect: #selector)
		hasSameElements: #(primitiveTableArray initializePrimitiveTable)
]

{ #category : #initialization }
DRCogitPrimitiveInstallerTest >> testingJITCompilerClass [

	^ interpreter environmentAt: super testingJITCompilerClass
]
