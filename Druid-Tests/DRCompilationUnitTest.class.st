Class {
	#name : #DRCompilationUnitTest,
	#superclass : #TestCase,
	#category : #'Druid-Tests-CompilerBuilder'
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

	self flag: #TODO. "Duplicated from compilationUnitForTesting"

	^ DRInterpreterCompilationUnit new
		compilerBuilder: (DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter);
		targetClass: self compilerClassName;
		yourself
]

{ #category : #'object access' }
DRCompilationUnitTest >> primitiveMethod [

	^ InterpreterPrimitives >> #primitiveAdd
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

	compilationUnit primitives: {
			(DRJITPrimitiveObject new
				 primitiveNumber: 1;
				 sourceMethod: self primitiveMethod;
				 argumentCount: 1;
				 yourself).

			(DRJITPrimitiveObject new
				 primitiveNumber: 2;
				 argumentCount: -1;
				 supported: false;
				 yourself).

			(DRJITPrimitiveObject new
				 primitiveNumber: 3;
				 argumentCount: 3;
				 supported: false;
				 mayCallback: true;
				 yourself) }.

	compilationUnit compileAll.

	self assert: self compilerClass primitiveTableArray equals: {
			#( 1 #gen_primitiveAdd 1 ).
			#( 2 #genNonImplementedPrimitive -1 ).
			#( 3 #genNonImplementedPrimitive 3 #maycallback ) }.
]

{ #category : #tests }
DRCompilationUnitTest >> testMaycallbackPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			                    primitiveNumber: 7;
			                    sourceMethod: self primitiveMethod;
			                    argumentCount: 1;
			                    mayCallback: true;
			                    yourself) }.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #gen_primitiveAdd 1 #maycallback ) }
]

{ #category : #tests }
DRCompilationUnitTest >> testNotSupportedPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			 primitiveNumber: 7;
			 argumentCount: -1;
			 supported: false;
			 yourself) }.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #genNonImplementedPrimitive -1 ) }.
	self deny: (self compilerClass canUnderstand: #gen_fakePrimitive)
]

{ #category : #tests }
DRCompilationUnitTest >> testPrimitiveCompilation [

	| compilationUnit |
	compilationUnit := self newEmptyCompilationUnit.

	compilationUnit primitives: { (DRJITPrimitiveObject new
			 primitiveNumber: 7;
			 sourceMethod: self primitiveMethod;
			 argumentCount: 1;
			 yourself) }.
	compilationUnit compileAll.

	self
		assert: self compilerClass primitiveTableArray
		equals: { #( 7 #gen_primitiveAdd 1 ) }
]
