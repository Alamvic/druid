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

{ #category : #accessing }
DRBytecodeCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	super configureForCompilerClass: aCompilerClass.
	self compilerClass: aCompilerClass.

	registerAllocator := compilerClass druidNewRegisterAllocator.
	codeGenerator := compilerClass druidNewCodeGenerator.
	optimisations := compilerClass druidNewOptimizationList
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
