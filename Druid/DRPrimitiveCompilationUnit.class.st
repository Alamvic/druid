Class {
	#name : #DRPrimitiveCompilationUnit,
	#superclass : #DRCompilationUnit,
	#instVars : [
		'primitives',
		'initPrimitiveTable',
		'interpreter',
		'dispatchTableBuilder',
		'primitiveMethods'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRPrimitiveCompilationUnit >> addPrimitive: aString [

	(self isValidPrimitiveName: aString)
		ifTrue: [ self primitives add: (self newPrimitive: aString) ].

]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> addPrimitiveEntries [
	"As part of the build unit, add the receiver's primitives from its <Collection> of <DRPrimitiveObject>"

	self compiler interpreter: self interpreter.
	self addSpecialPrimitives.
	self primitives do: [ :primitiveObject |
		self addPrimitiveEntry: primitiveObject ]
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> addPrimitiveEntry: aDRPrimitiveObject [
	"Add a primitive entry compatible with the expected format from #initializePrimitiveTable implementation"

	self initPrimitiveTable add: {
		aDRPrimitiveObject primitiveNumber .
		aDRPrimitiveObject genSelector .
		aDRPrimitiveObject argumentCount > 0
			ifTrue: [ aDRPrimitiveObject argumentCount ] }
]

{ #category : #adding }
DRPrimitiveCompilationUnit >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	(self collectPrimitives: self interpreterPrimitiveTable)
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> addSpecialPrimitives [
	"Private - Do nothing for now"
]

{ #category : #adding }
DRPrimitiveCompilationUnit >> collectPrimitives: aCollection [ 
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection 
		reject: #isNumber
		thenCollect: [ : symOrCompiledMethod | 
			symOrCompiledMethod isCompiledMethod 
				ifTrue: [ symOrCompiledMethod selector ]
				ifFalse: [ symOrCompiledMethod ] ].
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> compileAll [
	"Write primitive methods in the receiver's JIT compiler class"

	self primitives
			do: [ : primitive | self compilePrimitive: primitive ]
			displayingProgress: [ : primitive | 'Compiling primitive: ' , primitive asString ].
	self compileInitializePrimitiveTable
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> compileInitializePrimitiveTable [
	"Install the initialize primitive table method in the receiver's JIT compiler class"

	self addPrimitiveEntries.
	self dispatchTableBuilder installAllMethodsOn: self targetClass class
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> compilePrimitive: aDRPrimitiveObject [
	"Compile a method in the receiver's JIT compiler class. aDRPrimitiveObject contains the specification needed to compile the JITed version of the method"

	(self compiler
		initialize;
		sourceName: aDRPrimitiveObject sourceSelector;
		primitiveCompilerName: aDRPrimitiveObject genSelector)
			compile
]

{ #category : #private }
DRPrimitiveCompilationUnit >> dispatchTableBuilder [
	" Answer a <PCGMethodNode> representing the source code of the #initializePrimitiveTable compatible with Cogit "

	^ dispatchTableBuilder
		ifNil: [ dispatchTableBuilder := DRCogitDispatchTableBuilder primitiveTableMethodFrom: self ]
]

{ #category : #private }
DRPrimitiveCompilationUnit >> dispatchTableBuilder: aDRDispatchTableBuilder [
	" Set aDRDispatchTableBuilder to the receiver's dispatch table builder "

	dispatchTableBuilder := aDRDispatchTableBuilder
]

{ #category : #testing }
DRPrimitiveCompilationUnit >> hasPrimitives [
	"Answer <true> if the receiver already contains primitives"

	^ self primitives notEmpty
]

{ #category : #'accessing - compiler' }
DRPrimitiveCompilationUnit >> initPrimitiveTable [
	"Answer a <Colllection> of <Array>. Each Array specifies an entry in the primitive table as expected by Cogit>>initializePrimitiveTable implementation"

	^ initPrimitiveTable
		ifNil: [ initPrimitiveTable := OrderedCollection new ]
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> interpreter [

	^ interpreter
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> interpreter: aDRBasicInterpreter [ 

	interpreter := aDRBasicInterpreter
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> interpreterPrimitiveTable [

	^ self compilerBuilder interpreterPrimitiveTable 
]

{ #category : #testing }
DRPrimitiveCompilationUnit >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> newPrimitive: aString [
	" Answer a new primitive object with its metadata and CFG generated "

	^ DRPrimitiveObject new
		primitiveNumber: (self primitiveNumberOf: aString);
		sourceSelector: aString;
		cfg: (self compiler buildIR: aString);
		argumentCount: (self compiler numberOfArgumentsForSelector: aString);
		mayCallback: (self primitiveMayCallback: aString);
		yourself
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> primitiveMayCallback: aString [
	"Answer <true> if primitive aString is marked as maycallback"
	" #maycallback "

	^ aString = #genNonImplementedPrimitive
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> primitiveMethods [
	" Answer a <Collection> of <CompiledMethod> including primitives "

	^ primitiveMethods
		ifNil: [ primitiveMethods := OrderedCollection new ]
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> primitiveMethods: aCollection [

	primitiveMethods := aCollection
]

{ #category : #'accessing - primitives' }
DRPrimitiveCompilationUnit >> primitiveNumberOf: aSelector [
	" Answer a <Number> representing the primitive number of aSelector "

	| primNumber |

	primNumber := self interpreterPrimitiveTable indexOf: aSelector.
	^ self interpreterPrimitiveTable first = 0
		ifTrue: [ primNumber - 1 ]
		ifFalse: [ primNumber ]
]

{ #category : #accessing }
DRPrimitiveCompilationUnit >> primitives [
	"Answer a <Collection> of IR of primitive methods, instances of <DRPrimitiveControlFlowGraph>"

	^ primitives
		ifNil: [ primitives := OrderedCollection new ]
]
