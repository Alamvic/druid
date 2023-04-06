"
Implements a basic interpreter to compiler (it doesn't provide error handling capabilities) using interpreter primitives provided in a class. The interpreter to compiler generates a new class with Druid JITted versions of the primitive methods.

Its basic usage is:

```language=Pharo
| interpreterToCompiler |

"" Instantiate an interpreter to compiler providing an AbstractInterpreter subclass with primitives ""
interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.

"" Generate the Druid intermediate representation and compile methods in a new class ""
interpreterToCompiler generateIRAndCompileIn: #MyCoolJITCompilerClass

"" Browse the generated class ""
Smalltalk tools browser openOn: #MyCoolJITCompilerClass.
```



"
Class {
	#name : #DRInterpreterToCompiler,
	#superclass : #DRAbstractCompilerBuilder,
	#instVars : [
		'memory',
		'primitiveSelector',
		'failFirst',
		'primitiveCompiler',
		'bytecodeCompiler',
		'bytecodeSelector'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> fromInterpreterClass: anInterpreterClass [

	^ self new
		  interpreterClass: anInterpreterClass;
		  yourself
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> addBytecode: aString [

	[ self bytecodes add: (self newBytecode: aString) ]
	on: Error
	do: [ : ex | 
		failFirst ifTrue: [ ex pass  ].
		self bytecodeCompiler 
			failedBytecodesAt: ex
			add: self bytecodeCompiler sourceName ].

]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> addBytecodes [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	^ (self collectBytecodes: self interpreterBytecodeTable)
		  do: [ :prim | self addBytecode: prim ]
		  displayingProgress: [ :prim | 'Adding bytecode: ' , prim asString ]
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitive: aString [

	(self isValidPrimitiveName: aString)
		ifTrue: [ 
			[ self primitives add: (self newPrimitive: aString) ]
			on: Error
			do: [ : ex | 
				failFirst ifTrue: [ ex pass  ].
				self primitiveCompiler 
					failedPrimitivesAt: ex
					add: self primitiveCompiler sourceName ] ].

]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	^ (self collectPrimitives: self interpreterPrimitiveTable)
		do: [ : prim | self addPrimitive: prim ]
		displayingProgress: [ : prim | 'Adding primitive: ' , prim asString ]

		

]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> build [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self compilationUnit compiler: self primitiveCompiler.
	self addPrimitives.
	self compilationUnit compiler: self bytecodeCompiler.
	self addBytecodes.
	^ self compilationUnit compileAll
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass [
	"Generate and install the receiver's primitives and bytecodes into JIT compiler aClass"

	self compilationUnit targetClass: (self environmentAt: aClass).
	self build
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass superclass: aSuperclass [
	"Generate and install the receiver's primitives and bytecodes into JIT compiler aClass"

	self compilationUnit 
		targetSuperclass: aSuperclass;
		targetClass: (self environmentAt: aClass).
	self build

]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> bytecodeCompiler [

	^ (DRBytecodeCompilerCompiler forInterpreter: self newInterpreter)
		  compilerClass: self targetClass;
		  yourself
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodeNumberOf: aSelector [
	" Answer a <Number> representing the bytecode number of aSelector "

	^  self interpreterBytecodeTable indexOf: aSelector.

]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodes [

	^ self compilationUnit bytecodes
]

{ #category : #adding }
DRInterpreterToCompiler >> collectBytecodes: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection
		  reject: [ :obj |
			  obj isNil or: [
				  (self isSelectedBytecode: (interpreterClass lookupSelector: obj))
					  not ] ]
		  thenCollect: [ :symOrCompiledMethod |
			  symOrCompiledMethod isCompiledMethod
				  ifTrue: [ symOrCompiledMethod selector ]
				  ifFalse: [ symOrCompiledMethod ] ]
]

{ #category : #adding }
DRInterpreterToCompiler >> collectPrimitives: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection
		  reject: [ :obj |
			  obj isNumber or: [
				  obj isNil or: [ (self isSelectedPrimitive: (interpreterClass lookupSelector: obj)) not ] ] ]
		  thenCollect: [ :symOrCompiledMethod |
			  symOrCompiledMethod isCompiledMethod
				  ifTrue: [ symOrCompiledMethod selector ]
				  ifFalse: [ symOrCompiledMethod ] ]
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> compileAll [

	self build compileAll
]

{ #category : #initialization }
DRInterpreterToCompiler >> doFailOnFirst [

	failFirst := true
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> failedPrimitives [

	^ self primitiveCompiler failedPrimitives
]

{ #category : #'accessing - object memory' }
DRInterpreterToCompiler >> initializationOptions [

	^ {
		#BytesPerWord . 8 .
		#ObjectMemory .  memory class name
		}
]

{ #category : #initialization }
DRInterpreterToCompiler >> initialize [ 

	super initialize.
	primitiveSelector := [ : arg | true ].
	bytecodeSelector := [ : arg | false ].
	failFirst := false.
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> interpreterBytecodeTable [
	"Answer a <Collection> of <Symbol> each one representing a selector of the receiver's bytecode table"

	^ interpreterClass bytecodeTable
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass [

	^ interpreterClass
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass: anObject [

	interpreterClass := anObject.
	anObject initializePrimitiveTable.
	anObject initializeBytecodeTable.
	primitiveCompiler := nil.
	compilationUnit := nil.

]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^ interpreterClass primitiveTable
]

{ #category : #testing }
DRInterpreterToCompiler >> isExtendedBytecode: aSelector [
	" Answer true if bytecode described by aSelector is extended "
	
	^ self interpreterBytecodeTable 
]

{ #category : #adding }
DRInterpreterToCompiler >> isSelectedBytecode: obj [

	^ bytecodeSelector value: obj
]

{ #category : #adding }
DRInterpreterToCompiler >> isSelectedPrimitive: obj [

	^ primitiveSelector value: obj
]

{ #category : #testing }
DRInterpreterToCompiler >> isValidPrimitiveName: aString [

	^ aString isNumber not
]

{ #category : #adding }
DRInterpreterToCompiler >> newBytecode: aString [
	" Answer a new bytecode object with its metadata and CFG generated "

	| bytecodeObject |
	bytecodeObject := DRBytecodeObject new
		                  bytecodeNumber: (self bytecodeNumberOf: aString);
		                  sourceMethod:
			                  (self interpreterClass lookupSelector: aString);
		                  cfg: (self bytecodeCompiler
				                   sourceName: aString;
				                   compile);
		                  yourself.
	^ bytecodeObject
	" (self isExtendedBytecode: aString) 
		ifTrue: [ bytecodeObject beExtended ]."
]

{ #category : #'instance creation' }
DRInterpreterToCompiler >> newInterpreter [
	" Answer an <AbstractInterpreter|StackInterpreter> from the receiver's interpreter builder "

	memory := self newMemory.

	^ interpreterClass basicNew
		  objectMemory: memory;
		  basicInitialize
]

{ #category : #'accessing - object memory' }
DRInterpreterToCompiler >> newMemory [
	" Answer a memory manager instance. Currently a subclass of <SpurMemoryManager> "

	^ Spur64BitCoMemoryManager simulatorClass new
]

{ #category : #adding }
DRInterpreterToCompiler >> newPrimitive: aString [
	" Answer a new primitive object with its metadata and CFG generated "

	^ DRJITPrimitiveObject new
		primitiveNumber: (self primitiveNumberOf: aString);
		sourceMethod: (self interpreterClass lookupSelector: aString);
		cfg: (self primitiveCompiler buildIR: aString);
		argumentCount: (self primitiveCompiler numberOfArgumentsForSelector: aString);
		mayCallback: (self primitiveMayCallback: aString);
		yourself
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> primitiveCompiler [
	"Answer a Druid compiler configured for the receiver's interpreter builder"

	^ (DRPrimitiveCompilerCompiler forInterpreter: self newInterpreter)
				compilerClass: self targetClass;
				yourself
]

{ #category : #adding }
DRInterpreterToCompiler >> primitiveMayCallback: aString [
	"Answer <true> if primitive aString is marked as maycallback"
	" #maycallback "

	^ aString = #genNonImplementedPrimitive
]

{ #category : #adding }
DRInterpreterToCompiler >> primitiveNumberOf: aSelector [
	" Answer a <Number> representing the primitive number of aSelector "

	| primNumber |

	primNumber := self interpreterPrimitiveTable indexOf: aSelector.
	^ self interpreterPrimitiveTable first = 0
		ifTrue: [ primNumber - 1 ]
		ifFalse: [ primNumber ]
]

{ #category : #helpers }
DRInterpreterToCompiler >> primitiveTable [
	"Answer a <Collection> with the JITed primitives"

	^ compilationUnit primitiveTable
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives [

	^ self compilationUnit primitives
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives: aCollection [
	" Add a <Collection> of primitive selectors to the receiver's compilation unit "

	self compilationUnit primitives: aCollection asOrderedCollection.
	^ self compilationUnit primitives
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> selectBytecodes: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing bytecode methods and matching conditions in selectBlock "

	bytecodeSelector := selectBlock

]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "

	primitiveSelector := selectBlock

]
