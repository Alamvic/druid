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
		'compilerCompiler'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #'instance creation' }
DRInterpreterToCompiler class >> fromInterpreterClass: anInterpreterClass [

	^  self basicNew
		interpreterClass: anInterpreterClass;
		yourself
]

{ #category : #'accessing - model' }
DRInterpreterToCompiler >> build [
	"Answer a <DRCompilationUnit>, useful for model manipulation before code dumping"

	self compilationUnit 
		compiler: self compilerCompiler;
		addPrimitives.
	^ self compilationUnit
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

	self compilationUnit
		targetClass: (self environmentAt: aClass);
		compiler: self compilerCompiler;
		addPrimitives;
		compileAll
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

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass [

	^ interpreterClass
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterClass: anObject [

	interpreterClass := anObject
]

{ #category : #accessing }
DRInterpreterToCompiler >> interpreterPrimitiveTable [
	"Answer a <Collection> of <Symbol> each one representing a selector (e.g. #primitiveEqual) of the receiver's primitive table"

	^ interpreterClass primitiveTable
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

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitiveTable [
	"Answer a <Collection> of primitive selectors"

	^ self targetClass basicPrimitiveTable
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives [

	^ self compilationUnit primitives
		ifEmpty: [ self build primitives ]
		ifNotEmpty: [ : p | p ]
]

{ #category : #'accessing - primitives' }
DRInterpreterToCompiler >> primitives: aCollection [
	" Add a <Collection> of primitive selectors to the receiver's compilation unit "

	self compilationUnit primitives: aCollection asOrderedCollection.

]

{ #category : #accessing }
DRInterpreterToCompiler >> selectPrimitives: selectBlock [
	" Answer a <Collection> of <CompiledMethod> representing primitive methods and matching conditions in selectBlock "

	^ (self primitiveTable reject: #isNumber) 
		select: [ : primSelector | selectBlock value: (self interpreterClass lookupSelector: primSelector) ]
		thenCollect: [ : primSelector | self interpreterClass lookupSelector: primSelector ]
]
