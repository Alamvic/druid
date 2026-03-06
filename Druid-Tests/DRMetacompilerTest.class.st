Class {
	#name : 'DRMetacompilerTest',
	#superclass : 'DROptimisationTest',
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

{ #category : 'running' }
DRMetacompilerTest >> setUp [

	super setUp.
	compilerCompiler := self newCompiler
]
