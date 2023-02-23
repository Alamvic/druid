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

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	cfg validate.

	^ cfg
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
		optimisations add: DRInline new.
		optimisations add: DRSCCP new.
		optimisations add: DRCopyPropagation new.
		optimisations add: DRDeadCodeElimination new.
		optimisations add: DRCleanControlFlow new. ]
]

{ #category : #accessing }
DRCompilerCompiler >> irGenerator [

	^ irGenerator
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | anIR applyOptimisation: optimization ]
]
