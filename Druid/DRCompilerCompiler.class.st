Class {
	#name : #DRCompilerCompiler,
	#superclass : #Object,
	#instVars : [
		'interpreter',
		'compilerClass',
		'sourceName',
		'deferredBranches',
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
	
	"Remove the phi functions.
	From this point on, we are not in SSA form anymore"
	self resolvePhiFunctionsIn: druidIR.

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
	irGenerator interpretMethod: method ast receiver: (DRInterpreterValue value: interpreter).
	
	^ irGenerator ir
]

{ #category : #'as yet unclassified' }
DRCompilerCompiler >> generatePaths: cfg [
	cfg clearEdges.
	(DRPathFactory withCFG: cfg) generatePaths
]

{ #category : #'code-generation' }
DRCompilerCompiler >> generateTargetASTFromIR: aDRControlFlowGraph [

	^ codeGenerator generateTargetASTFromIR: aDRControlFlowGraph primitiveName: self primitiveCompilerName
]

{ #category : #'generation-IR' }
DRCompilerCompiler >> initialize [

	super initialize.
	
	codeGenerator := DRCogitCodeGenerator new.
	registerAllocator := DRLinearScanRegisterAllocator new.
	
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

{ #category : #'generation-IR' }
DRCompilerCompiler >> newIRGenerator [

	^ DRIRGenerator new
]

{ #category : #api }
DRCompilerCompiler >> optimize: anIR [

	optimisations do: [ :optimization | 
		anIR applyOptimization: optimization ]
]

{ #category : #compilation }
DRCompilerCompiler >> resolvePhiFunctionsIn: aDRControlFlowGraph [

	"Create a copy instruction for each of the values of the Phi function.
	Put the copy instruction at the end of the block generating the value.
	
	For example
	for t3 := phi(t1,t2)
		with t1 coming from block A and t2 coming from block B.
	create a copy t3 := t1 at the end of block A
	create a copy t3 := t2 at the end of block B"
	
	aDRControlFlowGraph instructions copy do: [ :i |
		i isPhiFunction ifTrue: [
			i removeFromCFG.
			i operands with: i basicBlock predecessors
				do: [ :value :originBlock | | copy |
					(value isKindOf: DRNullValue) ifFalse: [ 
						(value result isKindOf: DRNoRegister)
							ifFalse: [ 
								copy := DRCopy operands: { value result } result: i result.
								originBlock ifNotNil: [originBlock addInstruction: copy] ].
				] ] ] ]
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
