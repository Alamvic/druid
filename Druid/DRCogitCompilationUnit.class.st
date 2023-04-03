"
Concrete subclass implementing primitive compilation behavior. It provides behavior for:

- Adding primitives (`DRCompilationUnit>>#addPrimitiveEntries`, `DRCompilationUnit>>#addPrimitiveEntry:`).
- Compile added primitives (`DRCompilationUnit>>#compilePrimitive:`, `DRCompilationUnit>>#compileAll`).
- Configure the primitive table (`DRCompilationUnit>>#initPrimitiveTable`).
"
Class {
	#name : #DRCogitCompilationUnit,
	#superclass : #DRCompilationUnit,
	#instVars : [
		'primitives',
		'initPrimitiveTable',
		'bytecodes',
		'initBytecodeTable',
		'dispatchTableBuilder'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> addPrimitiveEntries [
	"As part of the build unit, add the receiver's primitives from its <Collection> of <DRPrimitiveObject>"

	self compiler interpreter: self interpreter.
	self addSpecialPrimitives.
	self primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ]
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject genSelector .
		aDRPrimitiveObject argumentCount > 0
			ifTrue: [ aDRPrimitiveObject argumentCount ] }
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	(self collectPrimitives: self interpreterPrimitiveTable)
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]

		

]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> addSpecialPrimitives [
	"Private - Do nothing for now"
]

{ #category : #accessing }
DRCogitCompilationUnit >> bytecodes [

	^ bytecodes
]

{ #category : #accessing }
DRCogitCompilationUnit >> bytecodes: anObject [

	bytecodes := anObject
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> collectPrimitives: aCollection [ 
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection 
		reject: [ : obj | obj isNumber or: [ obj isNil ] ]
		thenCollect: [ : symOrCompiledMethod | 
			symOrCompiledMethod isCompiledMethod 
				ifTrue: [ symOrCompiledMethod selector ]
				ifFalse: [ symOrCompiledMethod ] ].
]

{ #category : #'accessing - compiler' }
DRCogitCompilationUnit >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self compileInitializePrimitiveTable
]

{ #category : #'accessing - compiler' }
DRCogitCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self dispatchTableBuilder installAllMethodsOn: self targetClass class
]

{ #category : #'accessing - compiler' }
DRCogitCompilationUnit >> compilePrimitive: aDRPrimitiveObject [
	"Compile a method in the receiver's JIT compiler class. aDRPrimitiveObject contains the specification needed to compile the JITed version of the method"

	(self compiler
		initialize;
		sourceName: aDRPrimitiveObject sourceSelector;
		primitiveCompilerName: aDRPrimitiveObject genSelector)
			compile
]

{ #category : #private }
DRCogitCompilationUnit >> dispatchTableBuilder [
	" Answer a <PCGMethodNode> representing the source code of the #initializePrimitiveTable compatible with Cogit "

	^ dispatchTableBuilder
		ifNil: [ dispatchTableBuilder := self dispatchTableBuilderClass fromCompilationUnit: self ]
]

{ #category : #private }
DRCogitCompilationUnit >> dispatchTableBuilder: aDRDispatchTableBuilder [
	" Set aDRDispatchTableBuilder to the receiver's dispatch table builder "

	dispatchTableBuilder := aDRDispatchTableBuilder
]

{ #category : #private }
DRCogitCompilationUnit >> dispatchTableBuilderClass [ 

	^ DRCogitDispatchTableBuilder
]

{ #category : #testing }
DRCogitCompilationUnit >> hasPrimitives [
	"Answer <true> if the receiver already contains primitives"

	^ self primitives notEmpty
]

{ #category : #accessing }
DRCogitCompilationUnit >> initBytecodeTable [

	^ initBytecodeTable
]

{ #category : #accessing }
DRCogitCompilationUnit >> initBytecodeTable: anObject [

	initBytecodeTable := anObject
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> initPrimitiveTable [
	"Answer a <Colllection> of <Array>. Each Array specifies an entry in the primitive table as expected by Cogit>>initializePrimitiveTable implementation"

	^ initPrimitiveTable
		ifNil: [ initPrimitiveTable := OrderedCollection new ]
]

{ #category : #accessing }
DRCogitCompilationUnit >> initPrimitiveTable: anObject [

	initPrimitiveTable := anObject
]

{ #category : #accessing }
DRCogitCompilationUnit >> interpreter [

	^ self compiler interpreter
]

{ #category : #accessing }
DRCogitCompilationUnit >> interpreterClass [

	^ self interpreter class
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> interpreterPrimitiveTable [

	self interpreterClass initializePrimitiveTable.
	^ self interpreterClass basicPrimitiveTable 
]

{ #category : #testing }
DRCogitCompilationUnit >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> primitiveMayCallback: aString [
	"Answer <true> if primitive aString is marked as maycallback"
	" #maycallback "

	^ aString = #genNonImplementedPrimitive
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> primitiveNumberOf: aSelector [
	" Answer a <Number> representing the primitive number of aSelector "

	| primNumber |

	primNumber := self interpreterPrimitiveTable indexOf: aSelector.
	^ self interpreterPrimitiveTable first = 0
		ifTrue: [ primNumber - 1 ]
		ifFalse: [ primNumber ]
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]

{ #category : #'accessing - primitives' }
DRCogitCompilationUnit >> primitives: aCollection [
	"Set a <Collection> of primitive methods "

	primitives := aCollection
]
