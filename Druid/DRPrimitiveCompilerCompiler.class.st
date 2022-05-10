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

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> initialize [

	super initialize.

	optimisations := OrderedCollection new.
	optimisations add: DRBranchCollapse new.
	optimisations add: (DRDeadBlockElimination then: DRCleanControlFlow).

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [ 
		optimisations add: DRFailureCodeBasedTailDuplication new.
		optimisations add: (DRSCCP then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: (DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: (DRDeadPathElimination then: DRPhiSimplication) ]
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> newIRGenerator [

	^ DRPrimitiveIRGenerator new
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
