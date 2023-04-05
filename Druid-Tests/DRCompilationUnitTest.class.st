Class {
	#name : #DRCompilationUnitTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests-InterpreterCompiler'
}

{ #category : #tests }
DRCompilationUnitTest >> compilerClass [

	^ Smalltalk globals at: self compilerClassName
]

{ #category : #tests }
DRCompilationUnitTest >> compilerClassName [

	^ #MyJITCompilerClass
]

{ #category : #'instance creation' }
DRCompilationUnitTest >> newEmptyCompilationUnit [

	| interpreter |
	interpreter := CogVMSimulatorLSB basicNew
		               objectMemory: Spur64BitCoMemoryManager simulatorClass new;
		               basicInitialize.
	^ DRCompilationUnit new
		  interpreter: interpreter;
		  jitCompilerClass: self compilerClass;
		  yourself
]

{ #category : #running }
DRCompilationUnitTest >> setUp [

	super setUp.

	(StackToRegisterMappingCogit << self compilerClassName)
		tag: 'JIT';
		package: 'Druid-Tests-InterpreterCompiler';
		install
]

{ #category : #running }
DRCompilationUnitTest >> tearDown [

	super tearDown.

	Smalltalk globals removeClassNamed: self compilerClassName
]

{ #category : #tests }
DRCompilationUnitTest >> testManyPrimitivesCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit addPrimitive: (DRPrimitiveObject new
			 primitiveNumber: 1;
			 sourceSelector: #primitiveAdd;
			 argumentCount: 1;
			 yourself).

	compilationUnit addPrimitive: (DRPrimitiveObject new
			 primitiveNumber: 2;
			 sourceSelector: #fakePrimitive;
			 argumentCount: -1;
			 supported: false;
			 yourself).

	compilationUnit addPrimitive: (DRPrimitiveObject new
			 primitiveNumber: 3;
			 sourceSelector: #fakePrimitiveCallback;
			 argumentCount: 3;
			 supported: false;
			 mayCallback: true;
			 yourself).

	compilationUnit compileAll.

	self assert: self compilerClass primitiveTableArray equals: {
			#( 1 #gen_primitiveAdd 1 ).
			#( 2 #genNonImplementedPrimitive -1 ).
			#( 3 #genNonImplementedPrimitive 3 #maycallback ) }.
	self assert: (self compilerClass canUnderstand: #gen_primitiveAdd)
]

{ #category : #tests }
DRCompilationUnitTest >> testMaycallbackPrimitiveCompilation [

	| compilationUnit primitiveObject |
	compilationUnit := self newEmptyCompilationUnit.

	primitiveObject := DRPrimitiveObject new
		                   primitiveNumber: 7;
		                   sourceSelector: #primitiveAdd;
		                   argumentCount: 1;
		                   mayCallback: true;
		                   yourself.

	compilationUnit addPrimitive: primitiveObject.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #gen_primitiveAdd 1 #maycallback ) }.
	self assert: (self compilerClass >> #gen_primitiveAdd) isNotNil
]

{ #category : #tests }
DRCompilationUnitTest >> testNotSupportedPrimitiveCompilation [

	| compilationUnit primitiveObject |
	compilationUnit := self newEmptyCompilationUnit.

	primitiveObject := DRPrimitiveObject new
		                   primitiveNumber: 7;
		                   sourceSelector: #fakePrimitive;
		                   argumentCount: -1;
		                   supported: false;
		                   yourself.

	compilationUnit addPrimitive: primitiveObject.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #genNonImplementedPrimitive -1 ) }.
	self deny: (self compilerClass canUnderstand: #gen_fakePrimitive)
]

{ #category : #tests }
DRCompilationUnitTest >> testPrimitiveCompilation [

	| compilationUnit primitiveObject |
	compilationUnit := self newEmptyCompilationUnit.

	primitiveObject := DRPrimitiveObject new
		                   primitiveNumber: 7;
		                   sourceSelector: #primitiveAdd;
		                   argumentCount: 1;
		                   yourself.

	compilationUnit addPrimitive: primitiveObject.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #gen_primitiveAdd 1 ) }.
	self assert: (self compilerClass >> #gen_primitiveAdd) isNotNil
]
