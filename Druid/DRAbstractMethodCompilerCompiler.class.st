Class {
	#name : #DRAbstractMethodCompilerCompiler,
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
DRAbstractMethodCompilerCompiler >> compile [

	| druidIR |
	self customise.
	druidIR := self generateDruidIR.
	self optimize: druidIR.

	registerAllocator allocateRegistersIn: druidIR.

	targetAST := self generateTargetASTFromIR: druidIR.
	self writeAST: targetAST
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> configureIRBuilder: generator forMethod: method [

	| startBlock initialBasicBlock |

	"Create the initial basic block"
	startBlock := generator initialBasicBlock.
	initialBasicBlock := generator createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock.

	generator numberOfArguments: (self numberOfArgumentsFor: method)
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> customise [

	^ self customiseFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> customiseFor: method [

	method ast
		pragmaNamed: #customisedReceiverFor:
		ifPresent: [ :pragma | 
			customisation := DRPrimitiveCustomisation newCustomisation:
				                  pragma arguments first value ]
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> generateDruidIR [

	^ self generateDruidIRFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> generateDruidIRFor: method [

	"Generates and returns an intermediate representation of the primitive from the interpreter primitive"

	| cfg arguments |
	self configureIRBuilder: irGenerator forMethod: method.

	arguments := method argumentNames collect: [ :_ | irGenerator pop ].
	irGenerator
		interpretCode: (DRMethod methodNode: method ast)
		receiver: (DRInterpreterValue value: interpreter)
		arguments: arguments.

	cfg := irGenerator ir.

	"Clean garbage to generate a valid CFG"
	DRDeadBlockElimination new applyTo: cfg.
	DRDeadCodeElimination new applyTo: cfg.
	cfg validate.

	^ cfg
]

{ #category : #'code-generation' }
DRAbstractMethodCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	customisation generateCustomisedPreamble:
		codeGenerator generatorMethodBuilder.

	^ codeGenerator
		  generateTargetASTFromIR: aDRControlFlowGraph
		  methodName: self targetName
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> initialize [

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
DRAbstractMethodCompilerCompiler >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> interpreter: anObject [

	interpreter := anObject
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> irGenerator: aDRIRGenerator [

	irGenerator := aDRIRGenerator
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> numberOfArgumentsFor: method [

	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> optimisations: aCollection [ 
	
	optimisations := aCollection
]

{ #category : #api }
DRAbstractMethodCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | 
		anIR applyOptimisation: optimization ]
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> registerAllocator [
	^ registerAllocator
]

{ #category : #'generation-IR' }
DRAbstractMethodCompilerCompiler >> sourceMethod [

	^ interpreter class lookupSelector: sourceName
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> sourceName [

	^ sourceName
]

{ #category : #accessing }
DRAbstractMethodCompilerCompiler >> sourceName: anObject [

	sourceName := anObject
]

{ #category : #'code-generation' }
DRAbstractMethodCompilerCompiler >> targetName [

	self subclassResponsibility
]

{ #category : #'ast-to-source' }
DRAbstractMethodCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass compile: (aRBMethodNode formattedCode) classified: #generated
]
