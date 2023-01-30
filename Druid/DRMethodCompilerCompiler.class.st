Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRAbstractMethodCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArgumentsFor: aMethod [ 

	^ aMethod numArgs
]
