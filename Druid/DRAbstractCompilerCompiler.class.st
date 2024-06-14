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

	self flag: #TOFIX. "This customisation shoud be done between methods"
	self sourceMethods do: [ :method | self customiseFor: method ]
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

	^ self generateDruidIRForAll: self sourceMethods
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIRFor: compiledMethod [

	^ self generateDruidIRForAll: { compiledMethod }
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> generateDruidIRForAll: compiledMethods [
	"Generates and returns an intermediate representation of the primitive from the interpreter primitive"

	| cfg |
	irGenerator initializeIR.	
	self interpreteMethods: compiledMethods.
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

{ #category : #accessing }
DRAbstractCompilerCompiler >> instructionFactory [
	
	^ irGenerator instructionFactory
]

{ #category : #interpretation }
DRAbstractCompilerCompiler >> interpretMethods: compiledMethods receiver: aDRReceiver numArgs: numArgs [

	irGenerator receiver: aDRReceiver.
	irGenerator numberOfArguments: numArgs.

	compiledMethods do: [ :method |
		| aDRMethod arguments |
		aDRMethod := DRMethod methodNode: method ast.
		"???"
		arguments := aDRMethod methodNode argumentNames collect: [ :_ |
			             irGenerator pop ].
		
		irGenerator
			interpretCode: aDRMethod
			receiver: aDRReceiver
			arguments: arguments ]
]

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> interpreteMethods: compiledMethods [

	| receiver numArgs |
	receiver := DRInterpreterValue value: interpreter.
	numArgs := self numberOfArgumentsForManyMethods: compiledMethods.

	self
		interpretMethods: compiledMethods
		receiver: receiver
		numArgs: numArgs
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

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> numberOfArgumentsForManyMethods: compiledMethods [

	^ compiledMethods size = 1
		  ifTrue: [ self numberOfArgumentsForMethod: compiledMethods unique ]
		  ifFalse: [ 0 ]
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

{ #category : #'generation-IR' }
DRAbstractCompilerCompiler >> sourceMethods [

	self subclassResponsibility 
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
