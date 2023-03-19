Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRAbstractCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'ir-generation' }
DRMethodCompilerCompiler >> argName: index forMethod: method [

	^ method argumentNames at: index
]

{ #category : #'instance creation' }
DRMethodCompilerCompiler >> newIRGenerator [

	^ DRMethodIRGenerator new
]

{ #category : #'generation-IR' }
DRMethodCompilerCompiler >> numberOfArgumentsForMethod: aMethod [ 

	^ aMethod numArgs
]
