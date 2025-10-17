Class {
	#name : 'DRMetaCompilerCompiler',
	#superclass : 'DRAbstractCompilerCompiler',
	#instVars : [
		'customisation'
	],
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'generation-IR' }
DRMetaCompilerCompiler >> customiseFor: method [

	self subclassResponsibility 

]

{ #category : 'code-generation' }
DRMetaCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	customisation generateCustomisedPreamble:
		codeGenerator generatorMethodBuilder.

	^ super generateTargetASTFromIR: aDRControlFlowGraph
]

{ #category : 'initialization' }
DRMetaCompilerCompiler >> initialize [

	super initialize.
	customisation := DRNonePrimitiveCustomisation new
]

{ #category : 'code-generation' }
DRMetaCompilerCompiler >> newCodeGenerator [

	^ DRCogitCodeGenerator new
]

{ #category : 'api' }
DRMetaCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	super optimize: anIR
]
