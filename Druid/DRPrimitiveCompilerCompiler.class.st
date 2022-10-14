Class {
	#name : #DRPrimitiveCompilerCompiler,
	#superclass : #DRCompilerCompiler,
	#instVars : [
		'primitiveCompilerName',
		'fallthroughBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #API }
DRPrimitiveCompilerCompiler class >> compilePrimitive: primitiveName from: anInterpreter as: primitiveCompilerName in: aCompilerClass [ 
	
	^ self new
		primitiveName: primitiveName;
		interpreter: anInterpreter;
		primitiveCompilerName: primitiveCompilerName;
		compilerClass: aCompilerClass;
		compile
]

{ #category : #'generation-IR' }
DRPrimitiveCompilerCompiler >> configureIRBuilder: irGenerator forMethod: method [
	
	super configureIRBuilder: irGenerator forMethod: method.
	irGenerator numberOfArguments: (method ast pragmaNamed: #numberOfArguments:) arguments first value
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> createInitialBasicBlock [
	| initial initialBasicBlock |
	initialBasicBlock := super createInitialBasicBlock.
	irGenerator currentPrimitiveFailureCode: (initial := irGenerator instructionFactory copy: 0).
	initialBasicBlock addInstruction: initial.
	^ initialBasicBlock
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> initialize [

	super initialize.

	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).
	optimisations add: DRLoopInvariantCodeMotion new.

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [ 
		optimisations add: DRBranchCollapse new.
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCP then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: (DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: (DRDeadEdgeSplitting then: DRDeadBranchElimination).
		optimisations add: DRInline new. ].
	
	"Last Phase, make code coggable"
    optimisations add: DRCogitOperandSorter new.
	"optimisations add: DRCogitJITConstantSplitting new"
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> newIRGenerator [

	^ DRPrimitiveIRGenerator new
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> numberOfArguments: aMethod [ 
	^ (aMethod ast pragmaNamed: #numberOfArguments:) arguments first value
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveCompilerName [

	^ primitiveCompilerName
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveCompilerName: anObject [

	primitiveCompilerName := anObject
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveName [

	^ self sourceName
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> primitiveName: anObject [

	self sourceName: anObject
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> targetName [

	^ self primitiveCompilerName
]
