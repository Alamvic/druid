Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName',
		'fallthroughBlock'
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

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArguments: aMethod [ 

	^ aMethod numArgs
]
