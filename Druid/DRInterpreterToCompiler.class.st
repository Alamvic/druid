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
		'compilerCompilerClass',
		'compilerCompiler',
		'primitiveSelector',
		'failFirst'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> fromInterpreterClass: anInterpreterClass [

	^ self new
		  interpreterClass: anInterpreterClass;
		  yourself
]

{ #category : #adding }
DRInterpreterToCompiler >> addPrimitive: aString [

	(self isValidPrimitiveName: aString)
		ifTrue: [ 
			[ self primitives add: (self newPrimitive: aString) ]
			on: Error
			do: [ : ex | 
				failFirst ifTrue: [ ex pass  ].
				self compilerCompiler 
					failedPrimitivesAt: ex
					add: self compilerCompiler sourceName ] ].

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

	self compilationUnit compiler: self compilerCompiler.
	self addPrimitives.
	^ self compilationUnit compileAll
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass [
	"Generate and install the receiver's primitives into JIT compiler aClass"

	self build
		interpreter: interpreterClass;
		targetSuperclass: self targetSuperclass;
		targetClass: (self environmentAt: aClass);
		compileAll
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildAndCompileIn: aClass superclass: aSuperclass [
	"Generate and install the receiver's primitives into JIT compiler aClass"

	self build
		interpreter: interpreterClass;
		targetSuperclass: aSuperclass;
		targetClass: (self environmentAt: aClass);
		compileAll
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> buildIRAndCompileIn: aClass [
	"Generate and install the receiver's primitives into JIT compiler aClass"

	self compilationUnit targetClass: (self environmentAt: aClass).
	self build
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

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> compilerCompiler [
	"Answer a Druid compiler configured for the receiver's interpreter builder"

	^ compilerCompiler
		ifNil: [ compilerCompiler := 
			(self compilerCompilerClass forInterpreter: self newInterpreter)
					compilerClass: self targetClass;
					yourself ]
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> compilerCompilerClass [

	^ compilerCompilerClass
		ifNil: [ compilerCompilerClass := self defaultCompilerCompilerClass ]
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> compilerCompilerClass: aClass [

	compilerCompilerClass := aClass
]

{ #category : #'accessing - compiler' }
DRInterpreterToCompiler >> defaultCompilerCompilerClass [

	^ DRPrimitiveCompilerCompiler
]

{ #category : #initialization }
DRInterpreterToCompiler >> doFailOnFirst [

	failFirst := true
]

{ #category : #accessing }
DRInterpreterToCompiler >> failedPrimitives [

	^ self compilerCompiler failedPrimitives
]

{ #category : #helpers }
DRInterpreterToCompiler >> initPrimitiveTable [
	"Answer a <Collection> with the JITed primitives"

	^ compilationUnit initPrimitiveTable
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
	failFirst := false.
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass [

	^ interpreterClass
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass: anObject [

	interpreterClass := anObject.
	anObject initializePrimitiveTable.
	compilerCompiler := nil.
	compilationUnit := nil.

]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^ interpreterClass primitiveTable
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
		cfg: (self compilerCompiler buildIR: aString);
		argumentCount: (self compilerCompiler numberOfArgumentsForSelector: aString);
		mayCallback: (self primitiveMayCallback: aString);
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

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitiveTable [
	"Answer a <Collection> of primitive selectors"

	^ self targetClass basicPrimitiveTable
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

{ #category : #accessing }
DRInterpreterToCompiler >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "

	primitiveSelector := selectBlock

]
