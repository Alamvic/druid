Class {
	#name : #DRBytecodeCompilerCompiler,
	#superclass : #DRAbstractCompilerCompiler,
	#instVars : [
		'targetName',
		'bytecodes'
	],
	#category : #'Druid-BytecodeToJITCompilation'
}

{ #category : #API }
DRBytecodeCompilerCompiler class >> compilePrimitive: primitiveName from: anInterpreter as: primitiveCompilerName in: aCompilerClass [ 
	
	^ self new
		primitiveName: primitiveName;
		interpreter: anInterpreter;
		primitiveCompilerName: primitiveCompilerName;
		compilerClass: aCompilerClass;
		compile
]

{ #category : #'builder api' }
DRBytecodeCompilerCompiler >> buildIR: bytecodeSelector [ 
	" Answer a DRPrimitiveControlFlowGraph generated for bytecodeSelector in the receiver's interpreter "

	^ self 
		sourceName: bytecodeSelector;
		generateDruidIR
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> bytecodes: bytecodeAndSelectors [

	bytecodes := bytecodeAndSelectors
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	super configureForCompilerClass: aCompilerClass.
	self compilerClass: aCompilerClass.

	registerAllocator := compilerClass druidNewRegisterAllocator.
	codeGenerator := compilerClass druidNewCodeGenerator.
	optimisations := compilerClass druidNewOptimizationList
]

{ #category : #'generation-IR' }
DRBytecodeCompilerCompiler >> createInitialBasicBlock [
	| initial initialBasicBlock |
	
	"Probably to be removed? the bytecode does not have a failure code.
	Although some bytecode use primitive code and use the failure code!!"
	
	initialBasicBlock := super createInitialBasicBlock.
	irGenerator currentPrimitiveFailureCode: (initial := irGenerator instructionFactory copy: 0).
	initialBasicBlock addInstruction: initial.
	^ initialBasicBlock
]

{ #category : #'generation-IR' }
DRBytecodeCompilerCompiler >> generateDruidIR [

	bytecodes ifNotNil: [ 
		interpreter currentBytecode: bytecodes first key ].
	^ self generateDruidIRFor: self sourceMethods
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> newIRGenerator [

	^ DRBytecodeIRGenerator new
]

{ #category : #'generation-IR' }
DRBytecodeCompilerCompiler >> numberOfArgumentsForMethod: aMethod [

	(aMethod ast hasPragmaNamed: #numberOfArguments:) ifFalse: [ ^ 0 ].

	^ (aMethod ast pragmaNamed: #numberOfArguments:) arguments first value
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> sourceMethods [

	sourceName ifNotNil: [
		^ { (interpreter class lookupSelector: sourceName) } ].

	^ bytecodes asOrderedDictionary values collect: [ :selector |
		  interpreter class lookupSelector: selector ]
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> targetName [

	^ targetName 
		ifNil: [ targetName := 'gen_', self sourceName ]
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> targetName: anObject [

	targetName := anObject
]
