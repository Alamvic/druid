Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArgumentsFor: aMethod [ 

	^ aMethod numArgs
]
