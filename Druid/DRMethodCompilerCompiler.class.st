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

{ #category : #accessing }
DRMethodCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	super configureForCompilerClass: aCompilerClass.
	self compilerClass: aCompilerClass.

	codeGenerator := DRCogitCodeGenerator new.
	registerAllocator := DRCogitLinearScanRegisterAllocator new.
	customisation := DRNonePrimitiveCustomisation new.
	irGenerator := self newIRGenerator.

	optimisations := OrderedCollection new.

	optimisations add: DRInline new.
	2 timesRepeat: [ 
		optimisations add: DRDeadBlockElimination new.
		optimisations add: DRSCCP new.
		optimisations add: DRCopyPropagation new.
		optimisations add: DRDeadCodeElimination new.
		optimisations add: DRCleanControlFlow new. ]
]

{ #category : #'instance creation' }
DRMethodCompilerCompiler >> newIRGenerator [

	^ DRMethodIRGenerator new
]

{ #category : #'generation-IR' }
DRMethodCompilerCompiler >> numberOfArgumentsForMethod: aMethod [ 

	^ aMethod numArgs
]
