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
DRMethodCompilerCompiler >> numberOfArgumentsFor: aMethod [ 

	^ aMethod numArgs
]
