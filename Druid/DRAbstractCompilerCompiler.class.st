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
		'irGenerator',
		'compilerOptions'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'instance creation' }
DRAbstractCompilerCompiler class >> forInterpreter: anAbstractInterpreter [ 

	^ self new
		interpreter: anAbstractInterpreter;
		yourself
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> addCompilerOption: anOption [

	compilerOptions add: anOption
]

{ #category : #adding }
DRAbstractCompilerCompiler >> addCompilerOptions: aCollection [

	aCollection do: [ :e | self addCompilerOption: e ]
]

{ #category : #api }
DRAbstractCompilerCompiler >> allocateRegistersIn: druidIR [

	registerAllocator allocateRegistersIn: druidIR.
]

{ #category : #api }
DRAbstractCompilerCompiler >> compile [

	| druidIR |
	self customise.

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
DRAbstractCompilerCompiler >> compilerOptions [
	^ compilerOptions
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
	startBlock jumpTo: initialBasicBlock
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
				                 pragma arguments first value ]
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIR [

	^ self generateDruidIRFor: self sourceMethod
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIRFor: method [
	"Generates and returns an intermediate representation of the primitive from the interpreter primitive"

	| cfg receiver annotatedArgNums aMethod |
	self initializeIRBuilder: irGenerator.
	receiver := DRInterpreterValue value: interpreter.
	aMethod := DRMethod methodNode: method ast.

	annotatedArgNums := self numberOfArgumentsForMethod: method.

	annotatedArgNums isNumber
		ifTrue: [
			self
				interpretMethod: aMethod
				receiver: receiver
				numArgs: annotatedArgNums ]
		ifFalse: [
			self
				interpretMethod: aMethod
				receiver: receiver
				ManyNumArgs: annotatedArgNums ].


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
	irGenerator := self newIRGenerator.
	compilerOptions := Set new
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> initializeIRBuilder: generator [

	| startBlock initialBasicBlock |
	"Create the initial basic block"
	startBlock := generator initialBasicBlock.
	initialBasicBlock := generator createInitialBasicBlock.
	startBlock jumpTo: initialBasicBlock
]

{ #category : #accessing }
DRAbstractCompilerCompiler >> instructionFactory [
	
	^ irGenerator instructionFactory
]

{ #category : #interpretation }
DRAbstractCompilerCompiler >> interpretMethod: aDRMethod receiver: aDRReceiver ManyNumArgs: annotatedArgNums [

	| lastPreamble |
	lastPreamble := nil.
	annotatedArgNums do: [ :numArgs |
		lastPreamble ifNotNil: [ "Chain the checks"
			lastPreamble endInstruction falseBranch removePredecessor: lastPreamble.
			lastPreamble endInstruction newFalseBranch: irGenerator createInitialBasicBlock "Restart the VM state" ].

		lastPreamble := self generateCheckForNumArgs: numArgs.

		self
			interpretMethod: aDRMethod
			receiver: aDRReceiver
			numArgs: numArgs ]
]

{ #category : #interpretation }
DRAbstractCompilerCompiler >> interpretMethod: aDRMethod receiver: aDRReceiver numArgs: numArgs [

	| arguments |
	irGenerator receiver: aDRReceiver.
	irGenerator numberOfArguments: numArgs.
	"???"
	arguments := aDRMethod methodNode argumentNames collect: [ :_ |
		             irGenerator pop ].

	irGenerator
		interpretCode: aDRMethod
		receiver: aDRReceiver
		arguments: arguments
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

	^ DRIRGenerator new compiler: self
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
