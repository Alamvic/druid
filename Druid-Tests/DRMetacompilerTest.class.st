Class {
	#name : 'DRMetacompilerTest',
	#superclass : 'DRIRTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'running' }
DRMetacompilerTest >> configureInterpreter [

	compilerCompiler interpreter: DruidTestInterpreter basicNew 
]

{ #category : 'test' }
DRMetacompilerTest >> inputClass [

	^ self interpreterClass
]

{ #category : 'test' }
DRMetacompilerTest >> interpreterClass [

	^ DruidTestInterpreter
]

{ #category : 'running' }
DRMetacompilerTest >> newCompiler [

	^ DRPrimitiveCompilerCompiler new
]

{ #category : 'test' }
DRMetacompilerTest >> optimize: cfg [

	compilerCompiler optimize: cfg
]

{ #category : 'running' }
DRMetacompilerTest >> setUp [

	super setUp.
	compilerCompiler := self newCompiler
]
