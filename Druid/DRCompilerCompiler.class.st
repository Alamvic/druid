Class {
	#name : #DRCompilerCompiler,
	#superclass : #Object,
	#instVars : [
		'interpreter',
		'compilerClass',
		'sourceName',
		'registerAllocator',
		'targetAST',
		'optimisations',
		'codeGenerator',
		'customisation',
		'irGenerator'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #api }
DRCompilerCompiler >> compile [

	| druidIR |
	self customise.
	druidIR := self generateDruidIR.
	self optimize: druidIR.

	registerAllocator allocateRegistersIn: druidIR.

	targetAST := self generateTargetASTFromIR: druidIR.
	self writeAST: targetAST
]

{ #category : #accessing }
DRCompilerCompiler >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRCompilerCompiler >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> configureIRBuilder: generator forMethod: method [

	| startBlock initialBasicBlock |
	"Create the initial basic block"
	startBlock := generator initialBasicBlock.
	initialBasicBlock := generator createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock.

	generator receiver: (DRInterpreterValue value: interpreter).
	generator numberOfArguments: (self numberOfArgumentsFor: method)
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> customise [

	^ self customiseFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> customiseFor: method [

	method ast
		pragmaNamed: #customisedReceiverFor:
		ifPresent: [ :pragma | 
			customisation := DRPrimitiveCustomisation newCustomisation:
				                  pragma arguments first value ]
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> generateDruidIR [

	^ self generateDruidIRFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> generateDruidIRFor: method [
	"Generates and returns an intermediate representation of the primitive from the interpreter primitive"

	| cfg receiver arguments |
	self configureIRBuilder: irGenerator forMethod: method.

	arguments := method argumentNames collect: [ :_ | irGenerator pop ].
	receiver := DRInterpreterValue value: interpreter.

	irGenerator
		interpretCode: (DRMethod methodNode: method ast)
		receiver: receiver
		arguments: arguments.

	cfg := irGenerator ir.
1halt.
	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	cfg validate.

	^ cfg
]

{ #category : #'code-generation' }
DRCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	customisation generateCustomisedPreamble:
		codeGenerator generatorMethodBuilder.

	^ codeGenerator
		  generateTargetASTFromIR: aDRControlFlowGraph
		  methodName: self targetName
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> initialize [

	super initialize.

	codeGenerator := DRCogitCodeGenerator new.
	registerAllocator := DRCogitLinearScanRegisterAllocator new.
	customisation := DRNonePrimitiveCustomisation new.
	irGenerator := self newIRGenerator.

	optimisations := OrderedCollection new.

	2 timesRepeat: [ 
		optimisations add: DRDeadBlockElimination new.
		optimisations add: DRSCCP new.
		optimisations add: DRCopyPropagation new.
		optimisations add: DRDeadCodeElimination new.
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRInline new ]
]

{ #category : #accessing }
DRCompilerCompiler >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRCompilerCompiler >> interpreter: anObject [

	interpreter := anObject
]

{ #category : #accessing }
DRCompilerCompiler >> irGenerator [

	^ irGenerator
]

{ #category : #accessing }
DRCompilerCompiler >> irGenerator: aDRIRGenerator [

	irGenerator := aDRIRGenerator
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> numberOfArgumentsFor: method [

	^ self subclassResponsibility
]

{ #category : #accessing }
DRCompilerCompiler >> optimisations: aCollection [ 
	
	optimisations := aCollection
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | anIR applyOptimisation: optimization ]
]

{ #category : #accessing }
DRCompilerCompiler >> registerAllocator [
	^ registerAllocator
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> sourceMethod [

	^ interpreter class lookupSelector: sourceName
]

{ #category : #accessing }
DRCompilerCompiler >> sourceName [

	^ sourceName
]

{ #category : #accessing }
DRCompilerCompiler >> sourceName: anObject [

	sourceName := anObject
]

{ #category : #'code-generation' }
DRCompilerCompiler >> targetName [

	self subclassResponsibility
]

{ #category : #'ast-to-source' }
DRCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass compile: (aRBMethodNode formattedCode) classified: #generated
]
