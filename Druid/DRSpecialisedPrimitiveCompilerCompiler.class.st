Class {
	#name : #DRSpecialisedPrimitiveCompilerCompiler,
	#superclass : #DRPrimitiveCompilerCompiler,
	#instVars : [
		'specialisation'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> generateTargetASTFromIR: anIR [

	specialisation generateSpecialisedPreamble: codeGenerator generatorMethodBuilder .
	^ super generateTargetASTFromIR: anIR
]

{ #category : #initialization }
DRSpecialisedPrimitiveCompilerCompiler >> initialize [

	super initialize.
	specialisation := DRSmallIntegerSpecialisedPrimitive new
]

{ #category : #'generation-IR' }
DRSpecialisedPrimitiveCompilerCompiler >> optimize: anIR [

	specialisation optimize: anIR.
	super optimize: anIR
]
