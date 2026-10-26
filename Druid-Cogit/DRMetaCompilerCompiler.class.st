Class {
	#name : 'DRMetaCompilerCompiler',
	#superclass : 'DRAbstractCompilerCompiler',
	#instVars : [
		'customisation',
		'interpreter',
		'registerAllocator',
		'compilerClass'
	],
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'api' }
DRMetaCompilerCompiler >> allocateRegistersIn: druidIR [

	registerAllocator allocateRegistersIn: druidIR.
]

{ #category : 'accessing' }
DRMetaCompilerCompiler >> compilerClass [

	^ compilerClass
]

{ #category : 'accessing' }
DRMetaCompilerCompiler >> compilerClass: anObject [

	compilerClass := anObject
]

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

{ #category : 'generation-IR' }
DRMetaCompilerCompiler >> interpreteMethods: compiledMethods [

	| receiver numArgs |
	receiver := DRInterpreterValue value: interpreter.
	numArgs := self numberOfArgumentsForManyMethods: compiledMethods.

	self
		interpretMethods: compiledMethods
		receiver: receiver
		numArgs: numArgs
]

{ #category : 'accessing' }
DRMetaCompilerCompiler >> interpreter [

	^ interpreter
]

{ #category : 'accessing' }
DRMetaCompilerCompiler >> interpreter: anObject [

	interpreter := anObject
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

{ #category : 'accessing' }
DRMetaCompilerCompiler >> registerAllocator [
	^ registerAllocator
]

{ #category : 'generation-IR' }
DRMetaCompilerCompiler >> sourceMethod [
	" Answer a <CompiledMethod> matching the current receiver's sourceName in the interpreter class "

	^ interpreter class lookupSelector: sourceName
]

{ #category : 'generation-IR' }
DRMetaCompilerCompiler >> targetMethod [

	^ compilerClass lookupSelector: self targetName
]

{ #category : 'ast-to-source' }
DRMetaCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass
		compile: aRBMethodNode formattedCode
		classified: #generated
]
