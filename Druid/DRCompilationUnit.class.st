"
Abstract class to provide compilation behavior. Subclasses represents a Unit of compilation (a class), which can be asked for:

- Set a target class and its superclass (`DRCompilationUnit>>#targetClass:`, `DRCompilationUnit>>#targetSuperclass:`)..
- Adding primitives (`DRCompilationUnit>>#addPrimitiveEntries`, `DRCompilationUnit>>#addPrimitiveEntry:`).
- Compile added primitives (`DRCompilationUnit>>#compilePrimitive:`, `DRCompilationUnit>>#compileAll`).
- Configure the primitive table (`DRCompilationUnit>>#initPrimitiveTable`).

"
Class {
	#name : #DRCompilationUnit,
	#superclass : #Object,
	#instVars : [
		'primitives',
		'initPrimitiveTable',
		'interpreter',
		'dispatchTableBuilder',
		'primitiveMethods',
		'targetClass',
		'targetSuperclass'
	],
	#category : #'Druid-InterpreterBuilder'
}

{ #category : #accessing }
DRCompilationUnit >> addPrimitive: aDRPrimitiveObject [

	self primitives add: aDRPrimitiveObject
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> addPrimitiveEntries [
	"As part of the build unit, add the receiver's primitives from its <Collection> of <DRPrimitiveObject>"

	self compiler interpreter: self interpreter.
	self addSpecialPrimitives.
	self primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ]
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> addPrimitiveEntry: primitiveObject [

	self subclassResponsibility
]

{ #category : #accessing }
DRCompilationUnit >> addPrimitives: aCollection [

	self primitives addAll: aCollection
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> addSpecialPrimitives [

	self subclassResponsibility
]

{ #category : #'accessing - builder' }
DRCompilationUnit >> buildAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: 'Compiling primitives'.
	self buildInitializePrimitiveTable
]

{ #category : #'accessing - builder' }
DRCompilationUnit >> buildInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self primitiveMethods addAll: (self dispatchTableBuilder collectAllMethodsOn: self targetClass class)
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: [ : primitive | 'Compiling primitive: ' , primitive asString ].
	self compileInitializePrimitiveTable
]

{ #category : #'accessing - compiler' }
DRCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self dispatchTableBuilder installAllMethodsOn: self targetClass class
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
		compilerClass: self targetClass;
		yourself
]

{ #category : #private }
DRCompilationUnit >> dispatchTableBuilder [
	"Answer a <PCGMethodNode> representing the source code of the #initializePrimitiveTable compatible with Cogit"

	^ dispatchTableBuilder
		ifNil: [ dispatchTableBuilder := DRCogitDispatchTableBuilder primitiveTableMethodFrom: self ]
]

{ #category : #private }
DRCompilationUnit >> dispatchTableBuilder: aDRDispatchTableBuilder [
	"Set aDRDispatchTableBuilder to the receiver's dispatch table builder"

	dispatchTableBuilder := aDRDispatchTableBuilder
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
DRCompilationUnit >> primitiveMethods [

	^ primitiveMethods
		ifNil: [ primitiveMethods := OrderedCollection new ]
]

{ #category : #accessing }
DRCompilationUnit >> primitiveMethods: anObject [

	primitiveMethods := anObject
]

{ #category : #accessing }
DRCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]

{ #category : #accessing }
DRCompilationUnit >> targetClass [
	"Answer a <Class> which is the where the methods will be stored"

	^ targetClass
]

{ #category : #accessing }
DRCompilationUnit >> targetClass: anObject [

	targetClass := anObject
]

{ #category : #accessing }
DRCompilationUnit >> targetSuperclass [

	^ targetSuperclass
]

{ #category : #accessing }
DRCompilationUnit >> targetSuperclass: anObject [

	targetSuperclass := anObject
]
