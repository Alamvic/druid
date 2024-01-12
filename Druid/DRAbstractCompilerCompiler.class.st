Class {
	#name : #DRAbstractCompilerCompiler,
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

{ #category : #'instance creation' }
DRAbstractCompilerCompiler class >> forInterpreter: anAbstractInterpreter [ 

	^ self new
		interpreter: anAbstractInterpreter;
		yourself
]

{ #category : #api }
DRAbstractCompilerCompiler >> allocateRegistersIn: druidIR [

	registerAllocator allocateRegistersIn: druidIR.
]

{ #category : #api }
DRAbstractCompilerCompiler >> compile [

	| druidIR |
	self customise.
	1haltIf: [ sourceName =  #extPushCharacterBytecode ].
	druidIR := self generateDruidIR.
	self optimize: druidIR.

	self allocateRegistersIn: druidIR.

	targetAST := self generateTargetASTFromIR: druidIR.
	self writeAST: targetAST
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	"Hook for subclasses"
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> configureIRBuilder: generator forMethod: method [

	| startBlock initialBasicBlock |
	"Create the initial basic block"
	startBlock := generator initialBasicBlock.
	initialBasicBlock := generator createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock.

	"Config generator"
	generator receiver: (DRInterpreterValue value: interpreter).
	generator numberOfArguments:
		(self numberOfArgumentsForMethod: method).

]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> customise [

	^ self customiseFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> customiseFor: method [

	method ast
		pragmaNamed: #customisedReceiverFor:
		ifPresent: [ :pragma |
			customisation := DRPrimitiveCustomisation newCustomisation:
				                 pragma arguments first value ].

	method ast
		pragmaNamed: #checkArgumentCount
		ifPresent: [ :pragma | "TODO: Have a list of customisations"
			customisation := DRArgumentCountCheckCustomisation withArgumentCount: (self numberOfArgumentsForMethod: method) ]
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIR [

	^ self generateDruidIRFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIRFor: method [
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

{ #category : #'code-generation' }
DRAbstractCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	customisation generateCustomisedPreamble:
		codeGenerator generatorMethodBuilder.

	^ codeGenerator
		  generateTargetASTFromIR: aDRControlFlowGraph
		  methodName: self targetName
]

{ #category : #initialization }
DRAbstractCompilerCompiler >> initialize [

	super initialize.

	codeGenerator := DRCogitCodeGenerator new.
	customisation := DRNonePrimitiveCustomisation new.
	irGenerator := self newIRGenerator
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> instructionFactory [
	
	^ irGenerator instructionFactory
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> interpreter: anObject [

	interpreter := anObject
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> irGenerator [

	^ irGenerator
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> irGenerator: aDRIRGenerator [

	irGenerator := aDRIRGenerator
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> numberOfArgumentsForMethod: method [

	^ self subclassResponsibility
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> optimisations: aCollection [

	optimisations := aCollection
]

{ #category : #api }
DRAbstractCompilerCompiler >> optimize: anIR [

	customisation optimize: anIR.
	optimisations do: [ :optimization | anIR applyOptimisation: optimization ]
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> registerAllocator [
	^ registerAllocator
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> sourceMethod [
	" Answer a <CompiledMethod> matching the current receiver's sourceName in the interpreter class "

	^ interpreter class lookupSelector: sourceName
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> sourceName [

	^ sourceName
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> sourceName: anObject [

	sourceName := anObject
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> targetMethod [

	^ compilerClass lookupSelector: self targetName
]

{ #category : #'code-generation' }
DRAbstractCompilerCompiler >> targetName [

	self subclassResponsibility
]

{ #category : #'ast-to-source' }
DRAbstractCompilerCompiler >> writeAST: aRBMethodNode [ 
	
	compilerClass
		compile: aRBMethodNode formattedCode
		classified: #generated
]
