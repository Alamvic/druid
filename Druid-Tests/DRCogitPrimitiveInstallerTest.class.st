"
A DRCogitPrimitiveDumperTest is a test class for testing the behavior of DRCogitPrimitiveDumper
"
Class {
	#name : #DRCogitPrimitiveInstallerTest,
	#superclass : #TestCase,
	#instVars : [
		'primitiveMethod'
	],
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #running }
DRCogitPrimitiveInstallerTest >> compilationUnitForTesting [

	^ DRCompilationUnit new
]

{ #category : #running }
DRCogitPrimitiveInstallerTest >> setUp [

	super setUp.
	primitiveMethod := DRCogitPrimitiveInstaller primitiveTableMethodFrom: self compilationUnitForTesting
]

{ #category : #tests }
DRCogitPrimitiveInstallerTest >> testInitPrimitiveTable [

	self assert: primitiveMethod selector equals: #initializePrimitiveTable
]
