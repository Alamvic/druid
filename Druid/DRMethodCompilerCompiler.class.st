Class {
	#name : #DRMethodCompilerCompiler,
	#superclass : #DRCompilerCompiler,
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

{ #category : #accessing }
DRMethodCompilerCompiler >> numberOfArgumentsFor: aMethod [ 

	^ aMethod numArgs
]
