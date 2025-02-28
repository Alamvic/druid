"
Concrete subclass implementing primitive compilation behavior. It provides behavior for:

- Adding primitives (`DRCompilationUnit>>#addPrimitiveEntries`, `DRCompilationUnit>>#addPrimitiveEntry:`).
- Compile added primitives (`DRCompilationUnit>>#compilePrimitive:`, `DRCompilationUnit>>#compileAll`).
- Configure the primitive table (`DRCompilationUnit>>#initPrimitiveTable`).
"
Class {
	#name : 'DRInterpreterCompilationUnit',
	#superclass : 'DRCompilationUnit',
	#instVars : [
		'primitives',
		'bytecodes',
		'dispatchTableBuilder',
		'bytecodeTable',
		'primitiveTable'
	],
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'adding' }
DRInterpreterCompilationUnit >> addBytecodeEntries [

	self bytecodes do: [ :bytecodeObject |
		self addBytecodeEntry: bytecodeObject ].
]

{ #category : 'adding' }
DRInterpreterCompilationUnit >> addBytecodeEntry: aDRBytecodeObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self bytecodeTable addAll: aDRBytecodeObject byteCodeEntries
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> addPrimitiveEntries [
	"As part of the build unit, add the receiver's primitives from its <Collection> of <DRPrimitiveObject>"

	primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ].
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	| tableEntry |
	tableEntry := {
		              aDRPrimitiveObject primitiveNumber.
		              aDRPrimitiveObject genSelector }.
	aDRPrimitiveObject argumentCount ifNotNil: [ 
		 tableEntry := tableEntry, { aDRPrimitiveObject argumentCount } ].

	tableEntry := tableEntry , (aDRPrimitiveObject mayCallback
		               ifTrue: [ { #maycallback } ]
		               ifFalse: [ {  } ]).

	self primitiveTable add: tableEntry
]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> bytecodeTable [
	" Answer a <Collection> with the specification of the bytecodes in the receiver's interpreter "

	^ bytecodeTable

]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> bytecodes [

	^ bytecodes
		ifNil: [ bytecodes := OrderedCollection new ]
]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> bytecodes: anObject [

	bytecodes := anObject
]

{ #category : 'accessing - compiler' }
DRInterpreterCompilationUnit >> compileUsing: aCompiler [
	"Install methods in the receiver's JIT compiler class"

	| realTargetClass |
	realTargetClass := aCompiler environmentAt: self targetClass.
	self addPrimitiveEntries.
	self addBytecodeEntries.

	self bytecodes
		do: [ :e | aCompiler handleErrorDuring: [ e compileUsing: aCompiler ] ]
		displayingProgress: [ :e | 'Compiling bytecode ' , e genSelector ].
	primitives
		do: [ :e | aCompiler handleErrorDuring: [ e compileUsing: aCompiler ] ]
		displayingProgress: [ :e | 'Compiling primitive ' , e genSelector ].

	self dispatchTableGenerator installAllMethodsOn: realTargetClass class
]

{ #category : 'private' }
DRInterpreterCompilationUnit >> dispatchTableGenerator [
	" Answer a <PCGMethodNode> representing the source code of the #initializePrimitiveTable compatible with Cogit "

	^ dispatchTableBuilder
		ifNil: [ dispatchTableBuilder := self dispatchTableGeneratorClass fromCompilationUnit: self ]
]

{ #category : 'private' }
DRInterpreterCompilationUnit >> dispatchTableGenerator: aDRDispatchTableBuilder [
	" Set aDRDispatchTableBuilder to the receiver's dispatch table builder "

	dispatchTableBuilder := aDRDispatchTableBuilder
]

{ #category : 'private' }
DRInterpreterCompilationUnit >> dispatchTableGeneratorClass [ 

	^ DRCogitDispatchTableGenerator
]

{ #category : 'testing' }
DRInterpreterCompilationUnit >> hasPrimitives [
	"Answer <true> if the receiver already contains primitives"

	^ primitives notEmpty
]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> initialize [

	super initialize.
	bytecodeTable := OrderedCollection new.
	primitiveTable := OrderedCollection new.
	primitives := OrderedCollection new
]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> interpreter [

	^ self compiler interpreter
]

{ #category : 'accessing' }
DRInterpreterCompilationUnit >> interpreterClass [

	^ self interpreter class
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> interpreterPrimitiveTable [

	self interpreterClass initializePrimitiveTable.
	^ self interpreterClass basicPrimitiveTable 
]

{ #category : 'testing' }
DRInterpreterCompilationUnit >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> primitiveNumberOf: aSelector [
	" Answer a <Number> representing the primitive number of aSelector "

	| primNumber |

	primNumber := self interpreterPrimitiveTable indexOf: aSelector.
	^ self interpreterPrimitiveTable first = 0
		ifTrue: [ primNumber - 1 ]
		ifFalse: [ primNumber ]
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> primitiveTable [
	"Answer a <Colllection> of <Array>. Each Array specifies an entry in the primitive table as expected by Cogit>>initializePrimitiveTable implementation"

	^ primitiveTable
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
]

{ #category : 'accessing - primitives' }
DRInterpreterCompilationUnit >> primitives: aCollection [
	"Set a <Collection> of primitive methods "

	primitives := aCollection
]

{ #category : 'sorting' }
DRInterpreterCompilationUnit >> sortPrimitives [

	primitives sort: [ :a :b | a primitiveNumber < b primitiveNumber ]
]
