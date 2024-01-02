Class {
	#name : #DRInterpreterCompilationUnitTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #configuration }
DRInterpreterCompilationUnitTest >> bytecodeMethod [

	^ DruidTestInterpreter >> #bytecodeWithNoFrameInstVarRefAnnotation
]

{ #category : #configuration }
DRInterpreterCompilationUnitTest >> bytecodeMethodWithDescriptors [

	^ StackInterpreter >> #returnTopFromMethod
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

{ #category : #'object access' }
DRInterpreterCompilationUnitTest >> targetClass [

	^ DruidTestRTLCompiler
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testBytecodeCompilation [

	| compilationUnit bytecodes |
	compilationUnit := self newEmptyCompilationUnit.

	"Here the bytecode range is a single range sharing the same selector.
	That selector will be used for compilation"
	bytecodes := (self compiler newBytecodes:
		              (self compiler interpreterClass lookupSelector:
			               #storeAndPopReceiverVariableBytecode)) first: 4.
	compilationUnit bytecodes: bytecodes.
	self compile: compilationUnit.

	self assert: self targetClass bytecodeTable equals: {
			#( 1 200 200 #gen_StoreAndPopReceiverVariableBytecode0
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ).
			#( 1 201 201 #gen_StoreAndPopReceiverVariableBytecode1
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ).
			#( 1 202 202 #gen_StoreAndPopReceiverVariableBytecode2
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ).
			#( 1 203 203 #gen_StoreAndPopReceiverVariableBytecode3
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testBytecodesCompilationWithAnnotatedDescriptors [

	| compilationUnit bytecodes |
	compilationUnit := self newEmptyCompilationUnit.

	"Here the bytecode range is a single range sharing the same selector.
	That selector will be used for compilation"
	bytecodes := self compiler newBytecodes:
		             StackInterpreter >> #returnTopFromMethod.
	compilationUnit bytecodes: bytecodes.

	self compile: compilationUnit.

	self
		assert: self targetClass bytecodeTable
		equals:
			{ #( 1 92 92 #gen_ReturnTopFromMethod #return #isMappedInBlock
			   #needsFrameIfInBlock: -1 ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
]

{ #category : #'tests - bytecodes' }
DRInterpreterCompilationUnitTest >> testManyBytecodesCompilation [

	| compilationUnit bytecodes |
	compilationUnit := self newEmptyCompilationUnit.
	"Here the bytecode range is a single range sharing the same selector.
	That selector will be used for compilation"
	bytecodes := ((self compiler newBytecodes:
		               (self compiler interpreterClass lookupSelector:
			                #storeAndPopReceiverVariableBytecode)) first: 3)
	             , { (DRBytecodeObject new
			              bytecodeSize: 2;
			              bytecodeNumberStart: 3;
			              bytecodeNumberEnd: 5;
			              supported: false;
			              yourself) }.
	compilationUnit bytecodes: bytecodes.
	self compile: compilationUnit.

	self assert: self targetClass bytecodeTable equals: {
			#( 2 3 3 #unknownBytecode ).
			#( 2 4 4 #unknownBytecode ).
			#( 2 5 5 #unknownBytecode ).
			#( 1 200 200 #gen_StoreAndPopReceiverVariableBytecode0
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ).
			#( 1 201 201 #gen_StoreAndPopReceiverVariableBytecode1
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ).
			#( 1 202 202 #gen_StoreAndPopReceiverVariableBytecode2
			   #isInstVarRef #isMappedIfImmutability #needsFrameIfImmutability:
			   -1 ) }.

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
		equals: { #( 1 0 0 #gen_BytecodeWithNoFrameInstVarRefAnnotation #isInstVarRef #needsFrameNever: 1 ) }.

	self targetClass bytecodeTable do: [ :e |
		self assert: (self targetClass canUnderstand: e fourth) ]
]
