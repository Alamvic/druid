Class {
	#name : #DRInterpreterCompilationUnitTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #configuration }
DRInterpreterCompilationUnitTest >> bytecodeMethod [

	^ StackInterpreter >> #pushReceiverVariableBytecode
]

{ #category : #compiling }
DRInterpreterCompilationUnitTest >> compile: compilationUnit [

	| compiler |
	compiler := (DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter)
		            doFailOnFirst;
		            compilationUnit: compilationUnit;
		            yourself.
	compilationUnit compileUsing: compiler
]

{ #category : #'instance creation' }
DRInterpreterCompilationUnitTest >> newEmptyCompilationUnit [

	^ DRInterpreterCompilationUnit new
		  targetClass: self targetClass;
		  yourself
]

{ #category : #'object access' }
DRInterpreterCompilationUnitTest >> primitiveMethod [

	^ InterpreterPrimitives >> #primitiveAdd
]

{ #category : #running }
DRInterpreterCompilationUnitTest >> setUp [

	super setUp.

	(StackToRegisterMappingCogit << self targetClassName)
		tag: 'JIT';
		package: 'Druid-Tests-InterpreterCompiler';
		install
]

{ #category : #configuration }
DRInterpreterCompilationUnitTest >> targetClass [

	^ Smalltalk globals at: self targetClassName
]

{ #category : #configuration }
DRInterpreterCompilationUnitTest >> targetClassName [

	^ #MyJITCompilerClass
]

{ #category : #running }
DRInterpreterCompilationUnitTest >> tearDown [

	super tearDown.

	Smalltalk globals removeClassNamed: self targetClassName
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testBytecodeCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit bytecodes: { (DRBytecodeObject new
			 bytecodeSize: 1;
			 bytecodeNumberStart: 0;
			 bytecodeNumberEnd: 15;
			 sourceMethod: self bytecodeMethod;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass bytecodeTable
		equals: { #( 1 0 15 #gen_PushReceiverVariableBytecode ) }.

	self assert:
		(self targetClass canUnderstand: #gen_PushReceiverVariableBytecode)
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testManyBytecodesCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit bytecodes: {
			(DRBytecodeObject new
				 bytecodeSize: 1;
				 bytecodeNumberStart: 0;
				 bytecodeNumberEnd: 15;
				 sourceMethod: self bytecodeMethod;
				 yourself).
			(DRBytecodeObject new
				 bytecodeSize: 2;
				 bytecodeNumberStart: 16;
				 bytecodeNumberEnd: 30;
				 supported: false;
				 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self assert: self targetClass bytecodeTable equals: {
			#( 1 0 15 #gen_PushReceiverVariableBytecode ).
			#( 2 16 30 #unknownBytecode ) }.

	self assert:
		(self targetClass canUnderstand: #gen_PushReceiverVariableBytecode)
]

{ #category : #'tests - primitives' }
DRInterpreterCompilationUnitTest >> testManyPrimitivesCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: {
			(DRJITPrimitiveObject new
				 primitiveNumber: 1;
				 sourceMethod: self primitiveMethod;
				 argumentCount: 1;
				 yourself).

			(DRJITPrimitiveObject new
				 primitiveNumber: 2;
				 argumentCount: -1;
				 sourceMethod: self primitiveMethod;
				 supported: false;
				 yourself).

			(DRJITPrimitiveObject new
				 primitiveNumber: 3;
				 argumentCount: 3;
				 supported: false;
				 sourceMethod: self primitiveMethod;
				 mayCallback: true;
				 yourself) } asOrderedCollection.

	self compile: compilationUnit.


	self assert: self targetClass primitiveTableArray equals: {
			#( 1 #gen_PrimitiveAdd 1 ).
			#( 2 #genNonImplementedPrimitive -1 ).
			#( 3 #genNonImplementedPrimitive 3 #maycallback ) }.

	self assert: (self targetClass canUnderstand: #gen_PrimitiveAdd)
]

{ #category : #'tests - primitives' }
DRInterpreterCompilationUnitTest >> testMaycallbackPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			 primitiveNumber: 7;
			 sourceMethod: self primitiveMethod;
			 argumentCount: 1;
			 mayCallback: true;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass primitiveTableArray
		equals: { #( 7 #gen_PrimitiveAdd 1 #maycallback ) }.
		
	self assert: (self targetClass canUnderstand: #gen_PrimitiveAdd)
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testNotSupportedBytecodeCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit bytecodes: { (DRBytecodeObject new
			 bytecodeSize: 1;
			 bytecodeNumberStart: 0;
			 bytecodeNumberEnd: 15;
			 supported: false;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass bytecodeTable
		equals: { #( 1 0 15 #unknownBytecode ) }
]

{ #category : #'tests - primitives' }
DRInterpreterCompilationUnitTest >> testNotSupportedPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			 primitiveNumber: 7;
			 argumentCount: -1;
			 supported: false;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass primitiveTableArray
		equals: { #( 7 #genNonImplementedPrimitive -1 ) }
]

{ #category : #'tests - primitives' }
DRInterpreterCompilationUnitTest >> testPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			 primitiveNumber: 7;
			 sourceMethod: self primitiveMethod;
			 argumentCount: 1;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass primitiveTableArray
		equals: { #( 7 #gen_PrimitiveAdd 1 ) }.

	self assert: (self targetClass canUnderstand: #gen_PrimitiveAdd)
]
