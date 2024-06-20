Class {
	#name : #DRPrimitiveCompilerCompiler,
	#superclass : #DRAbstractCompilerCompiler,
	#instVars : [
		'primitiveCompilerName'
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

{ #category : #'ir-generation' }
DRPrimitiveCompilerCompiler >> argName: index forMethod: method [

	^ 'Arg' , index asString
]

{ #category : #'initialize-release' }
DRPrimitiveCompilerCompiler >> beForSimpleStack [
	
	codeGenerator := DRCogitSimpleStackPrimitiveCodeGenerator new
]

{ #category : #'initialize-release' }
DRPrimitiveCompilerCompiler >> beForStackToRegisterMapping [

	codeGenerator := DRCogitStackToRegisterPrimitiveCodeGenerator new
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> buildIR: primitiveSelector [ 
	" Answer a DRPrimitiveControlFlowGraph generated for primitiveSelector in the receiver's interpreter "

	^ self 
		sourceName: primitiveSelector;
		generateDruidIR
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	super configureForCompilerClass: aCompilerClass.
	self compilerClass: aCompilerClass.

	registerAllocator := DRCogitLinearScanRegisterAllocator new.
	codeGenerator := compilerClass druidNewPrimitiveCodeGenerator.
	optimisations := StackToRegisterMappingCogit druidNewOptimizationList
]

{ #category : #'ir-generation' }
DRPrimitiveCompilerCompiler >> generateCheckForNumArgs: numArgs [

	| block checkArgCount |
	block := irGenerator currentBasicBlock.

	checkArgCount := block
		                 equals: DRInterpreterReference argumentCount
		                 to: numArgs asDRValue.

	block
		jumpIf: checkArgCount
		to: irGenerator newBasicBlock "Continue the interpretation from here"
		ifFalseTo: irGenerator failureExitBlock. 

	^ block
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> initialize [

	super initialize.
	
	registerAllocator := DRCogitLinearScanRegisterAllocator new.

	optimisations := OrderedCollection new.
	optimisations add: (DRCleanControlFlow then: DRCopyPropagation).
	optimisations add: DRLoopInvariantCodeMotion new.

	"Basic set of optimizations twice for primitives"
	3 timesRepeat: [ 
		optimisations add: DRBranchCollapse new.
		"optimisations add: DRFailureCodeBasedTailDuplication new."
		optimisations add: (DRSCCP then: DRDeadBlockElimination).
		optimisations add: DRCopyPropagation new.
		optimisations add: DRPhiSimplication new.
		optimisations add: DRCogitCanonicaliser new.
		optimisations add: (DRDeadCodeElimination then: DRRedundantCopyElimination).
		optimisations add: DRCleanControlFlow new.
		optimisations add: (DRDeadEdgeSplitting then: DRDeadBranchElimination).
		optimisations add: DRInline new. ].
	
	"Last Phase, make code coggable"
    optimisations add: DRCogitOperandSorter new
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> newIRGenerator [

	^ DRPrimitiveIRGenerator new compiler: self
]

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> numberOfArguments [
	
	^ self numberOfArgumentsForMethod: self sourceMethod
]

{ #category : #'generation-IR' }
DRPrimitiveCompilerCompiler >> numberOfArgumentsForMethod: anObject [
	"Answer a <Number> representing the number of arguments found in anObject (method or selector in the receiver's primitive's source)"

	^  (anObject ast pragmaNamed: #numberOfArguments:) arguments first value
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

{ #category : #accessing }
DRPrimitiveCompilerCompiler >> targetName: aString [ 
	
	self primitiveCompilerName: aString
]
