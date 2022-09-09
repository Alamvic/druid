Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName',
		'fallthroughBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArguments: aMethod [ 

	^ aMethod numArgs
]
