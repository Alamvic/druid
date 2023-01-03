Class {
	#name : #DRBytecodeCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
	],
	#category : #'Druid-CompilerCompiler'
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
		optimisations add: (DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: (DRDeadEdgeSplitting then: DRDeadBranchElimination) ].
	
	"Last Phase, make code coggable"
    optimisations add: DRCogitOperandSorter new.
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
