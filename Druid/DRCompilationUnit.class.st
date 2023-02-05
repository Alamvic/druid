"
It represents a Unit of compilation (a class).

"
Class {
	#name : #DRCompilationUnit,
	#superclass : #Object,
	#instVars : [
		'primitives',
		'jitCompilerClass',
		'initPrimitiveTable',
		'interpreter'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRCompilationUnit >> addPrimitive: aDRPrimitiveObject [

	self primitives add: aDRPrimitiveObject 
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> addPrimitiveEntries [

	self compiler interpreter: self interpreter.
	self primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ]
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject genSelector .
		aDRPrimitiveObject argumentCount > 0
			ifTrue: [ aDRPrimitiveObject argumentCount ] }
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: 'Compiling primitives'.
	self compileInitializePrimitiveTable
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self primitiveTableInitializer installAllMethodsOn:
		self jitCompilerClass class
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compilePrimitive: aDRPrimitiveObject [
	"Compile a method in the receiver's JIT compiler class. aDRPrimitiveObject contains the specification needed to compile the JITed version of the method"

	| intrpSelector jitSelector |
	intrpSelector := aDRPrimitiveObject sourceSelector.
	jitSelector := aDRPrimitiveObject genSelector.
	(self compiler
		interpreter: self interpreter;
		primitiveName: intrpSelector;
		primitiveCompilerName: jitSelector)
			compile
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compiler [

	^ DRPrimitiveCompilerCompiler new
		compilerClass: self jitCompilerClass;
		yourself
]

{ #category : #testing }
DRCompilationUnit >> hasPrimitives [
	"Answer <true> if the receiver already contains primitives"

	^ self primitives notEmpty
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> initPrimitiveTable [
	"Answer a <Colllection> of <Array>. Each Array specifies an entry in the primitive table as expected by Cogit>>initializePrimitiveTable implementation"

	^ initPrimitiveTable
		ifNil: [ initPrimitiveTable := OrderedCollection new ]
]

{ #category : #accessing }
DRCompilationUnit >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRCompilationUnit >> interpreter: aDRBasicInterpreter [ 

	interpreter := aDRBasicInterpreter
]

{ #category : #accessing }
DRCompilationUnit >> jitCompilerClass [
	"Answer a <Class> which is the where the JITed methods will be stored"

	^ jitCompilerClass
]

{ #category : #accessing }
DRCompilationUnit >> jitCompilerClass: anObject [

	jitCompilerClass := anObject
]

{ #category : #private }
DRCompilationUnit >> primitiveTableInitializer [
	"Answer a <PCGMethodNode> representing the source code of the #initializePrimitiveTable compatible with Cogit"

	^ DRCogitPrimitiveInstaller primitiveTableMethodFrom: self
]

{ #category : #accessing }
DRCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]
