Class {
	#name : 'DRBytecodeCompilerCompiler',
	#superclass : 'DRAbstractCompilerCompiler',
	#instVars : [
		'targetName',
		'bytecodes'
	],
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'API' }
DRBytecodeCompilerCompiler class >> compilePrimitive: primitiveName from: anInterpreter as: primitiveCompilerName in: aCompilerClass [ 
	
	^ self new
		primitiveName: primitiveName;
		interpreter: anInterpreter;
		primitiveCompilerName: primitiveCompilerName;
		compilerClass: aCompilerClass;
		compile
]

{ #category : 'builder api' }
DRBytecodeCompilerCompiler >> buildIR: bytecodeSelector [ 
	" Answer a DRPrimitiveControlFlowGraph generated for bytecodeSelector in the receiver's interpreter "

	^ self 
		sourceName: bytecodeSelector;
		generateDruidIR
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> bytecodes: bytecodeAndSelectors [

	bytecodes := bytecodeAndSelectors
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> configureForCompilerClass: aCompilerClass [

	super configureForCompilerClass: aCompilerClass.
	self compilerClass: aCompilerClass.

	registerAllocator := compilerClass druidNewRegisterAllocator.
	codeGenerator := compilerClass druidNewCodeGenerator.
	optimisations := compilerClass druidNewOptimizationList
]

{ #category : 'generation-IR' }
DRBytecodeCompilerCompiler >> customiseFor: method [

	self flag: #TODO. "Allow mix of customisations"
	super customiseFor: method.

	"Avoid the customisation if we are compiling the super instruction"
	(self compilerOptions includes: #superInstructions) ifTrue: [
		self sourceMethods size > 1 ifFalse: [
			method ast
				pragmaNamed: #nextSuperInstructions:
				ifPresent: [ :pragma |
					bytecodes ifNil: [ self error: 'Missing current bytecode' ].
					customisation := DRSuperInstructionCustomisation
						                 interpreter: interpreter
						                 compilerClass: compilerClass
						                 currentBytecode: bytecodes unique key
						                 nextBytecodes: pragma arguments first value ] ] ]
]

{ #category : 'generation-IR' }
DRBytecodeCompilerCompiler >> generateDruidIR [

	bytecodes ifNotNil: [ 
		interpreter currentBytecode: bytecodes last key ].
	^ self generateDruidIRForAll: self sourceMethods
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> newIRGenerator [

	^ DRBytecodeIRGenerator new compiler: self
]

{ #category : 'generation-IR' }
DRBytecodeCompilerCompiler >> numberOfArgumentsForMethod: aMethod [

	(aMethod ast hasPragmaNamed: #numberOfArguments:) ifFalse: [ ^ 0 ].

	^ (aMethod ast pragmaNamed: #numberOfArguments:) arguments first value
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> optimize: anIR [

	super optimize: anIR.
	anIR applyOptimisation: DRAnnotateBytecodeInstructionCoalescing new
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> sourceMethods [

	sourceName ifNotNil: [
		^ { (interpreter class lookupSelector: sourceName) } ].

	^ bytecodes asOrderedDictionary values collect: [ :selector |
		  interpreter class lookupSelector: selector ]
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> sourceName [

	^ super sourceName ifNil: [ self sourceMethods unique selector ]
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> targetName [

	^ targetName 
		ifNil: [ targetName := 'gen_', self sourceName ]
]

{ #category : 'accessing' }
DRBytecodeCompilerCompiler >> targetName: anObject [

	targetName := anObject
]
