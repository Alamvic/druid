Class {
	#name : #DRCompilerCompiler,
	#superclass : #Object,
	#instVars : [
		'interpreter',
		'compilerClass',
		'sourceName',
		'registerAllocator',
		'druidIR',
		'targetAST',
		'optimisations',
		'codeGenerator'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #api }
DRCompilerCompiler >> compile [

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
DRCompilerCompiler >> generateDruidIR [

	| method |
	method := interpreter class lookupSelector: sourceName.
	^ self generateDruidIRFor: method
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

	^ codeGenerator generateTargetASTFromIR: aDRControlFlowGraph primitiveName: self primitiveCompilerName
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> initialize [

	super initialize.
	
	codeGenerator := DRCogitCodeGenerator new.
	registerAllocator := DRCogitLinearScanRegisterAllocator new.
	
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

	optimisations do: [ :optimization | 
		anIR applyOptimisation: optimization ]
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
	
	compilerClass compile: aRBMethodNode formattedCode classified: #generated
]
