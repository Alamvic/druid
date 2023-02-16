Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRAbstractCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArgumentsFor: aMethod [ 

	^ aMethod numArgs
]
