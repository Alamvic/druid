Class {
	#name : #DRBytecodeCompilerCompiler,
	#superclass : #DRAbstractCompilerCompiler,
	#instVars : [
		'targetName'
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

{ #category : #accessing }
DRBytecodeCompilerCompiler >> initialize [

	super initialize.


	(compilerClass isKindOf: StackToRegisterMappingCogit)
		ifTrue: [
		registerAllocator := DRCogitDynamicLinearScanRegisterAllocator new ]
		ifFalse: [
		registerAllocator := DRCogitLinearScanRegisterAllocator new ].

	(compilerClass isKindOf: StackToRegisterMappingCogit)
		ifTrue: [
		codeGenerator := DRCogitStackToRegisterMappingGenerator new ]
		ifFalse: [ codeGenerator := DRCogitSimpleStackGenerator new ].

	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [
		optimisations add: DRBranchCollapse new.
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCP then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRCogitCanonicaliser new.
		optimisations add:
			(DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: DRTailDuplicateStagedExpressions new.
		optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination) ].

	"Last Phase, make code coggable"
	optimisations add: DRCogitOperandSorter new.

	(compilerClass isKindOf: StackToRegisterMappingCogit) ifTrue: [
		optimisations add: DRCogitSendMarshallInsertion new.
		optimisations add: DRCogitStackToRegisterFlushHoisting new.

		registerAllocator := DRCogitDynamicLinearScanRegisterAllocator new ]
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
DRBytecodeCompilerCompiler >> targetName [

	^ targetName 
		ifNil: [ targetName := 'gen_', self sourceName ]
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> targetName: anObject [

	targetName := anObject
]
