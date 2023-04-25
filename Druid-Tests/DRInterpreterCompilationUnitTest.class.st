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
	compiler := self compiler
		            compilationUnit: compilationUnit;
		            yourself.
	compilationUnit compileUsing: compiler
]

{ #category : #compiling }
DRInterpreterCompilationUnitTest >> compiler [

	^ (DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter)
		  doFailOnFirst;
		  yourself
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

	| compilationUnit bytecodes |
	compilationUnit := self newEmptyCompilationUnit.

	"Here the bytecode range is a single range sharing the same selector.
	That selector will be used for compilation"
	bytecodes := self compiler newBytecodes:
		             StackInterpreter >> #pushReceiverVariableBytecode.
	compilationUnit bytecodes: bytecodes.
	self compile: compilationUnit.

	self assert: self targetClass bytecodeTable equals: {
			#( 1 0 0 #gen_pushReceiverVariableBytecode0 ).
			#( 1 1 1 #gen_pushReceiverVariableBytecode1 ).
			#( 1 2 2 #gen_pushReceiverVariableBytecode2 ).
			#( 1 3 3 #gen_pushReceiverVariableBytecode3 ).
			#( 1 4 4 #gen_pushReceiverVariableBytecode4 ).
			#( 1 5 5 #gen_pushReceiverVariableBytecode5 ).
			#( 1 6 6 #gen_pushReceiverVariableBytecode6 ).
			#( 1 7 7 #gen_pushReceiverVariableBytecode7 ).
			#( 1 8 8 #gen_pushReceiverVariableBytecode8 ).
			#( 1 9 9 #gen_pushReceiverVariableBytecode9 ).
			#( 1 10 10 #gen_pushReceiverVariableBytecode10 ).
			#( 1 11 11 #gen_pushReceiverVariableBytecode11 ).
			#( 1 12 12 #gen_pushReceiverVariableBytecode12 ).
			#( 1 13 13 #gen_pushReceiverVariableBytecode13 ).
			#( 1 14 14 #gen_pushReceiverVariableBytecode14 ).
			#( 1 15 15 #gen_pushReceiverVariableBytecode15 ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testManyBytecodesCompilation [

	| compilationUnit bytecodes |
	compilationUnit := self newEmptyCompilationUnit.

	"Here the bytecode range is a single range sharing the same selector.
	That selector will be used for compilation"
	bytecodes := self compiler newBytecodes: self bytecodeMethod.
	compilationUnit bytecodes: bytecodes , { (DRBytecodeObject new
			 bytecodeSize: 2;
			 bytecodeNumberStart: 16;
			 bytecodeNumberEnd: 17;
			 supported: false;
			 yourself) }.
	self compile: compilationUnit.

	self assert: self targetClass bytecodeTable equals: {
			#( 1 0 0 #gen_pushReceiverVariableBytecode0 ).
			#( 1 1 1 #gen_pushReceiverVariableBytecode1 ).
			#( 1 2 2 #gen_pushReceiverVariableBytecode2 ).
			#( 1 3 3 #gen_pushReceiverVariableBytecode3 ).
			#( 1 4 4 #gen_pushReceiverVariableBytecode4 ).
			#( 1 5 5 #gen_pushReceiverVariableBytecode5 ).
			#( 1 6 6 #gen_pushReceiverVariableBytecode6 ).
			#( 1 7 7 #gen_pushReceiverVariableBytecode7 ).
			#( 1 8 8 #gen_pushReceiverVariableBytecode8 ).
			#( 1 9 9 #gen_pushReceiverVariableBytecode9 ).
			#( 1 10 10 #gen_pushReceiverVariableBytecode10 ).
			#( 1 11 11 #gen_pushReceiverVariableBytecode11 ).
			#( 1 12 12 #gen_pushReceiverVariableBytecode12 ).
			#( 1 13 13 #gen_pushReceiverVariableBytecode13 ).
			#( 1 14 14 #gen_pushReceiverVariableBytecode14 ).
			#( 1 15 15 #gen_pushReceiverVariableBytecode15 ).
			#( 2 16 16 #unknownBytecode ).
			#( 2 17 17 #unknownBytecode ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
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
			 bytecodeNumberEnd: 0;
			 supported: false;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass bytecodeTable
		equals: { #( 1 0 0 #unknownBytecode ) }
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

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testSingleBytecodesCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit bytecodes: { (DRBytecodeObject new
			 bytecodeSize: 1;
			 bytecodeNumberStart: 0;
			 bytecodeNumberEnd: 0;
			 sourceMethod: self bytecodeMethod;
			 yourself) } asOrderedCollection.

	self compile: compilationUnit.

	self
		assert: self targetClass bytecodeTable
		equals: { #( 1 0 0 #gen_PushReceiverVariableBytecode ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
]
