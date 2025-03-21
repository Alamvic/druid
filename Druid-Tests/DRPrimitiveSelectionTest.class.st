Class {
	#name : 'DRPrimitiveSelectionTest',
	#superclass : 'DRAbstractCompilerCollectorTest',
	#instVars : [
		'primitiveMethods',
		'interpreterToCompiler'
	],
	#category : 'Druid-Tests-CompilerBuilder',
	#package : 'Druid-Tests',
	#tag : 'CompilerBuilder'
}

{ #category : 'running' }
DRPrimitiveSelectionTest >> setUp [

	super setUp.
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.
	interpreterToCompiler
		targetClass: self jitCompilerClassForTest;
		targetSuperclass: CogVMSimulatorLSB
]

{ #category : 'tests' }
DRPrimitiveSelectionTest >> testSelectionMultipleParams [

	interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #var:type: setting: #('value' #sqLong) ].
	interpreterToCompiler build.
	self assert: (interpreterToCompiler primitives allSatisfy: [ : cm | cm hasPragmaNamed: #var:type: ]).
	self assert: (interpreterToCompiler primitives allSatisfy: [ : cm | ((cm pragmaAt: #var:type:) argumentNamed: #var:) = 'value' ]).
	self assert: (interpreterToCompiler primitives allSatisfy: [ : cm | ((cm pragmaAt: #var:type:) argumentNamed: #type:) = 'sqLong' ])
]

{ #category : 'tests' }
DRPrimitiveSelectionTest >> testSelectionNoParam [

	interpreterToCompiler selectPrimitives: [ :p |
		p hasPragmaNamed: #api ].
	interpreterToCompiler build.
	self assert: (interpreterToCompiler primitives allSatisfy: [ :cm |
			 cm hasPragmaNamed: #api ])
]

{ #category : 'tests' }
DRPrimitiveSelectionTest >> testSelectionNumberOfArguments [

	interpreterToCompiler selectPrimitives: [ :p | p hasPragmaNamed: #numberOfArguments: setting: 1 ].
	interpreterToCompiler build.

	primitiveMethods := interpreterToCompiler primitives.
	self deny: primitiveMethods isEmpty.
	self assert: (primitiveMethods allSatisfy: [ :cm |
			 (cm hasPragmaNamed: #numberOfArguments:) and: [
				 ((cm sourceMethod pragmaAt: #numberOfArguments:) argumentNamed:  #numberOfArguments:) = 1 ] ])
]

{ #category : 'tests' }
DRPrimitiveSelectionTest >> testSelectionSingleParam [

	interpreterToCompiler selectPrimitives: [ : p | p hasPragmaNamed: #inline: setting: true ].
	interpreterToCompiler build.
	primitiveMethods := interpreterToCompiler primitives.
	self assert: (primitiveMethods allSatisfy: [ : cm |
		(cm hasPragmaNamed: #inline:) and: [ ((cm pragmaAt: #inline:) argumentNamed: #inline:) ] ])
]
