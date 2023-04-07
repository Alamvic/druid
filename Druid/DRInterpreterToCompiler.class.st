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
		'bytecodeCompiler',
		'bytecodeSelector',
		'errors'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> fromInterpreterClass: anInterpreterClass [

	^ self new
		  interpreterClass: anInterpreterClass;
		  yourself
]

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> generateDruidJITModel [

	<script>
	| compilerClassName interpreterCompiler compilationUnit primitives |
	compilerClassName := #DruidJIT.
	primitives := #( #primitiveAdd #primitiveSubtract #primitiveLessThan
	                 #primitiveGreaterThan #primitiveLessOrEqual
	                 #primitiveGreaterOrEqual #primitiveEqual #primitiveNotEqual
	                 #primitiveMultiply #primitiveDivide ).

	interpreterCompiler := self
		                       fromInterpreter:
		                       (DRDummyInterpreter new primitives: primitives)
		                       compilerClass: DRPrimitiveCompilerCompilerCollector.

	compilationUnit := interpreterCompiler generateIR.
	interpreterCompiler generateBuildModelAndCompileIn: compilerClassName
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> addBytecodes: aSpecArray [
	"Iterate over the receiver's interpreter (assumed to contain bytecodes methods) and add them to compilationUnit"

	^ (self collectBytecodes: aSpecArray)
		do: [ : bytecodePerSize | 
			| bytecodeSize |
			bytecodeSize := bytecodePerSize first.
			self bytecodes addAll: (bytecodePerSize second collect:  [ : bytecodeSpec |
				self bytecodesFromSpec: bytecodeSpec size: bytecodeSize ]) ]
		displayingProgress: 'Adding bytecodes'

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> addBytecodes [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	^ (self collectBytecodes: self interpreterBytecodeTable)
		  do: [ :prim | self addBytecode: prim ]
		  displayingProgress: [ :prim | 'Adding bytecode: ' , prim asString ]
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitive: primitiveMethod [

	(self isValidPrimitiveName: primitiveMethod) ifFalse: [ 1 halt ].

	[ self primitives add: (self newPrimitive: primitiveMethod) ]
		on: Error
		do: [ :ex |
			failFirst ifTrue: [ ex pass ].
			self
				failedPrimitivesAt: ex
				add: primitiveMethod selector ]
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitives [
	"Iterate over the receiver's interpreter (assumed to contain primitive methods) and add them to compilationUnit"

	(self collectPrimitives: self interpreterPrimitiveTable)
		do: [ :prim | self addPrimitive: prim ]
		displayingProgress: [ :prim | 'Adding primitive: ' , prim selector ].

	self flag: #TOFIX.
	self interpreterPrimitiveTable
		do: [ :selector |
			(selector isNumber or: [ self primitives anySatisfy: [ :e | e sourceSelector = selector ] ])
				ifFalse: [
					| method |
					method := self interpreterClass lookupSelector: selector.
					(self isMaycallbackPrimitive: method) ifTrue: [
						self primitives add: (self newNonImplementedPrimitive: method) ] ] ]
		displayingProgress: [ :selector | 'Adding mandatory non implemented primitive: ' , selector asString ]
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> build [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self addPrimitives.
	self addBytecodes: self interpreterClass bytecodeTableArray.
	^ self compilationUnit compileUsing: self
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

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodeNumberOf: aSelector [
	" Answer a <Number> representing the bytecode number of aSelector "

	^  self interpreterBytecodeTable indexOf: aSelector.

]

{ #category : #constants }
DRInterpreterToCompiler >> bytecodeTable [
	"Answer a <Collection> containing <Array> with each receiver bytecode's specification" 
	
	^ self compilationUnit bytecodeTable
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodes [

	^ self compilationUnit bytecodes
]

{ #category : #'accessing - bytecodes' }
DRInterpreterToCompiler >> bytecodesFromSpec: aCollection size: size [ 
	" Answer a <Collection> of <DRBytecodeObject> using aCollection as specification "
	| start end  |
	
	start := aCollection first.
	end := aCollection second isNumber
		ifTrue: [ aCollection second ]
		ifFalse: [ aCollection first ].
	^ (start to: end) collect: [ : index | 
		| bytecodeSpecSelector |
		bytecodeSpecSelector := aCollection detect: [ : elem | elem isNumber not ].
		self 
			newBytecode: bytecodeSpecSelector
			index: index 
			size: size  ]
]

{ #category : #adding }
DRInterpreterToCompiler >> collectBytecodes: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	^ aCollection
		  collect: [ : specsArray |
				Array
					with: specsArray first
					with: (specsArray second select: [ : specArray | 
						| bytecodeSpecArraySelector |
						bytecodeSpecArraySelector := specArray detect: [ : obj | obj isNumber not ].
						(self isSelectedBytecode: (interpreterClass lookupSelector: bytecodeSpecArraySelector)) ]) ]
]

{ #category : #adding }
DRInterpreterToCompiler >> collectPrimitives: aCollection [
	" Answer a <Collection> of selectors from aCollection containing CompiledMethod or primitive names "

	| validSelectors |
	validSelectors := aCollection reject: [ :obj | obj isNumber or: [ obj isNil ] ].

	^ validSelectors
		  collect: [ :selector | interpreterClass lookupSelector: selector ]
		  thenSelect: [ :method | self isSelectedPrimitive: method ]
]

{ #category : #initialization }
DRInterpreterToCompiler >> doFailOnFirst [

	failFirst := true
]

{ #category : #adding }
DRInterpreterToCompiler >> errors [

	^ errors
		ifNil: [
			errors :=
				PluggableDictionary new
					equalBlock: [ :a :b | a class == b class ];
					hashBlock: [ :a | a class identityHash ];
					yourself 	]
]

{ #category : #adding }
DRInterpreterToCompiler >> failedBytecodesAt: anException add: aString [

	self errors
		at: anException
		ifPresent: [ : fbs | fbs add: aString ]
		ifAbsent: [ self errors 
			at: anException 
			put: (OrderedCollection with: aString) ]
]

{ #category : #adding }
DRInterpreterToCompiler >> failedPrimitivesAt: anException add: aString [

	"Put all errors in a single place"
	self failedBytecodesAt:  anException add: aString
]

{ #category : #private }
DRInterpreterToCompiler >> handleErrorDuring: aFullBlockClosure [

	aFullBlockClosure
		on: Error
		do: [ :ex |
			failFirst ifTrue: [ ex pass ].
			self failedBytecodesAt: ex add: ex asString ]
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

{ #category : #testing }
DRInterpreterToCompiler >> isMaycallbackPrimitive: primitiveMethod [

	^ primitiveMethod hasPragmaNamed: #maycallback
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

{ #category : #'instance creation' }
DRInterpreterToCompiler >> newBytecode: aString index: anInteger size: size [
	" Answer a new bytecode object setting its metadata "

	^ DRBytecodeObject new
		bytecodeNumber: anInteger;
		bytecodeSize: size;
		sourceMethod: (self interpreterClass lookupSelector: aString);
		yourself.

]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> newBytecodeCompiler [

	^ (DRBytecodeCompilerCompiler forInterpreter: self newInterpreter)
		compilerClass: self targetClass;
		yourself
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
DRInterpreterToCompiler >> newNonImplementedPrimitive: interpreterPrimitiveMethod [

	| selector |
	selector := interpreterPrimitiveMethod selector.

	self flag: #TODO. "Duplicated code with newPrimitive:"

	^ DRJITPrimitiveObject new
		  primitiveNumber: (self primitiveNumberOf: selector);
		  sourceMethod: interpreterPrimitiveMethod;
		  argumentCount: -1;
		  mayCallback:
			  (self isMaycallbackPrimitive: interpreterPrimitiveMethod);
		  supported: false;
		  yourself
]

{ #category : #'instance creation' }
DRInterpreterToCompiler >> newPrimitive: primitiveMethod [
	" Answer a new primitive object with its metadata and CFG generated "

	| selector primCompiler |
	primCompiler := self newPrimitiveCompiler.
	selector := primitiveMethod selector.
	^ DRJITPrimitiveObject new
		  primitiveNumber: (self primitiveNumberOf: selector);
		  sourceMethod: primitiveMethod;
		  argumentCount:
			  (primCompiler numberOfArgumentsForMethod: primitiveMethod);
		  mayCallback: (self isMaycallbackPrimitive: primitiveMethod);
		  yourself
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> newPrimitiveCompiler [
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
