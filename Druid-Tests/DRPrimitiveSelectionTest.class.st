Class {
	#name : #DRPrimitiveSelectionTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'primitiveMethods',
		'interpreterToCompiler'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #running }
DRPrimitiveSelectionTest >> setUp [

	super setUp.
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.
	interpreterToCompiler
		compilerCompilerClass: #MyJITCompilerClass;
		targetSuperclass: CogVMSimulatorLSB
]

{ #category : #tests }
DRPrimitiveSelectionTest >> testSelectionMultipleParams [

	primitiveMethods := interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #var:type: setting: #('value' #sqLong) ].
	self assert: (primitiveMethods allSatisfy: [ : cm | cm hasPragmaNamed: #var:type: ]).
	self assert: (primitiveMethods allSatisfy: [ : cm | ((cm pragmaAt: #var:type:) argumentNamed: #var:) = 'value' ]).
	self assert: (primitiveMethods allSatisfy: [ : cm | ((cm pragmaAt: #var:type:) argumentNamed: #type:) = 'sqLong' ])
]

{ #category : #tests }
DRPrimitiveSelectionTest >> testSelectionNoParam [

	primitiveMethods := interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #api ].
	self assert: (primitiveMethods allSatisfy: [ : cm |
		(cm hasPragmaNamed: #api) ])
]

{ #category : #tests }
DRPrimitiveSelectionTest >> testSelectionNumberOfArguments [

	primitiveMethods := interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #numberOfArguments: setting: 1 ].
	self assert: (primitiveMethods allSatisfy: #isCompiledMethod).
	self assert: (primitiveMethods allSatisfy: [ : cm |
		(cm hasPragmaNamed: #numberOfArguments:) and: [ ((cm pragmaAt: #numberOfArguments:) argumentNamed: #numberOfArguments:) = 1 ] ])
]

{ #category : #tests }
DRPrimitiveSelectionTest >> testSelectionSingleParam [

	primitiveMethods := interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #inline: setting: true ].
	self assert: (primitiveMethods allSatisfy: #isCompiledMethod).
	self assert: (primitiveMethods allSatisfy: [ : cm |
		(cm hasPragmaNamed: #inline:) and: [ ((cm pragmaAt: #inline:) argumentNamed: #inline:) ] ])
]
