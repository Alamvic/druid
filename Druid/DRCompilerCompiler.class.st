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
		'customisation'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #api }
DRCompilerCompiler >> compile [

	| druidIR |
	self customise.
	druidIR := self generateDruidIR.
	self optimize: druidIR.
	1halt.

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
	| irGenerator initialBasicBlock initial startBlock |
	irGenerator := self newIRGenerator.

	"Create the initial basic block"
	startBlock := irGenerator initialBasicBlock.
	initialBasicBlock := irGenerator newBasicBlock.
	startBlock jumpTo: initialBasicBlock.

	irGenerator currentPrimitiveFailureCode: (initial := irGenerator instructionFactory copy: 0).
	initialBasicBlock addInstruction: initial.
	
	irGenerator numberOfArguments: (method ast pragmaNamed: #numberOfArguments:) arguments first value.
	irGenerator interpretMethod: (DRMethod methodNode: method ast) receiver: (DRInterpreterValue value: interpreter).
	
	^ irGenerator ir
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
	
	optimisations := OrderedCollection new.
	optimisations add: DRBranchCollapse new.
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

{ #category : #'generation-IR' }
DRCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | 
		anIR applyOptimisation: optimization ]
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
	
	compilerClass compile: (aRBMethodNode formattedCodeIn: self class) classified: #generated
]
