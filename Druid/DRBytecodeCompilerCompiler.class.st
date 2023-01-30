Class {
	#name : #DRBytecodeCompilerCompiler,
	#superclass : #DRAbstractMethodCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
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

	codeGenerator := DRCogitStackToRegisterMappingGenerator new.
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
		"optimisations add:
			(DRDeadEdgeSplitting then: DRDeadBranchElimination)" ].

	"Last Phase, make code coggable"
	optimisations add: DRCogitOperandSorter new.
	optimisations add: DRCogitSendMarshallHoisting new
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> newIRGenerator [

	^ DRBytecodeIRGenerator new
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> numberOfArgumentsFor: aMethod [

	(aMethod ast hasPragmaNamed: #numberOfArguments:) ifFalse: [ ^ 0 ].

	^ (aMethod ast pragmaNamed: #numberOfArguments:) arguments first value
]

{ #category : #accessing }
DRBytecodeCompilerCompiler >> targetName [

	^ 'gen_', self sourceName
]
