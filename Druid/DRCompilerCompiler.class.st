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
DRCompilerCompiler >> createInitialBasicBlock [
	^ irGenerator newBasicBlock
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
	| initialBasicBlock startBlock cfg numArgs arguments loadArgumentInst |
	
	"Create the initial basic block"
	startBlock := irGenerator initialBasicBlock.
	initialBasicBlock := self createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock.
	
	numArgs := (self numberOfArguments: method).
	irGenerator numberOfArguments: numArgs.
	arguments := OrderedCollection new.
	(numArgs > 0) ifTrue: [1 to: numArgs do: [ :i |
		 loadArgumentInst := "TODO: Use instructionFactory to create Result register"
					DRLoadArgument new operands: { i }; yourself.
		 initialBasicBlock addInstruction: loadArgumentInst.
		 arguments add: loadArgumentInst ]].
	
	irGenerator interpretCode: (DRMethod methodNode: method ast) receiver: (DRInterpreterValue value: interpreter) arguments: arguments.
	
	cfg := irGenerator ir.

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	cfg validate.

	^cfg
]

{ #category : #'code-generation' }
DRCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	customisation generateCustomisedPreamble:
		codeGenerator generatorMethodBuilder.

	^ codeGenerator
		  generateTargetASTFromIR: aDRControlFlowGraph
		  primitiveName: self primitiveCompilerName
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> initialize [

	super initialize.
	
	codeGenerator := DRCogitCodeGenerator new.
	registerAllocator := DRCogitLinearScanRegisterAllocator new.
	customisation := DRNonePrimitiveCustomisation new.
	irGenerator := self newIRGenerator.
	
	optimisations := OrderedCollection new.
	optimisations add: DRDeadBlockElimination new.
	optimisations add: DRSCCP new.
	optimisations add: DRCopyPropagation new.
	optimisations add: DRDeadCodeElimination new.
	optimisations add: DRCleanControlFlow new.
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
DRCompilerCompiler >> irGenerator: aDRIRGenerator [

	irGenerator := aDRIRGenerator
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #accessing }
DRCompilerCompiler >> optimisations: aCollection [ 
	
	optimisations := aCollection
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | 
		anIR applyOptimisation: optimization ]
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

{ #category : #'ast-to-source' }
DRCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass compile: (aRBMethodNode formattedCode) classified: #generated
]
