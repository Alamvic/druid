Class {
	#name : #DRInterpreterBuilderTest,
	#superclass : #DRInterpreterToCompilerTest,
	#instVars : [
		'druidInterpreter'
	],
	#category : #'Druid-Tests-InterpreterBuilder'
}

{ #category : #tests }
DRInterpreterBuilderTest >> interpreterClassForTest [

	^ testingEnvironment at: self interpreterClassNameForTest 
]

{ #category : #tests }
DRInterpreterBuilderTest >> interpreterClassNameForTest [

	^ #DRNewInterpreterForTest
]

{ #category : #running }
DRInterpreterBuilderTest >> setUp [

	super setUp.
	interpreterToCompiler := DRInterpreterBuilderToCompiler new
		targetSuperclass: DRBasicCogInterpreterArithmeticPrimitives;
		yourself.

]

{ #category : #running }
DRInterpreterBuilderTest >> tearDown [

	druidInterpreter
		ifNotNil: [ testingEnvironment removeClassNamed: druidInterpreter class name ].
	super tearDown
]

{ #category : #tests }
DRInterpreterBuilderTest >> testAddPrimitives [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveEqual).
	druidInterpreter := interpreterToCompiler newInterpreter.
	interpreterToCompiler addPrimitives: primitiveSet.

	self
		assertCollection: (interpreterToCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterToCompiler primitives allSatisfy: [ : p | p isKindOf: DRPrimitiveObject ])
]

{ #category : #tests }
DRInterpreterBuilderTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	interpreterToCompiler generateIRAndCompileIn: self interpreterClassNameForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterBuilderTest >> testConfigureInterpreterClass [

	interpreterToCompiler targetClass: self interpreterClassNameForTest.
	self deny: (interpreterToCompiler targetClass respondsTo: #basicPrimitiveTable).
	
	interpreterToCompiler 
		addPrimitives: #(#primitiveAdd);
		compileAll.
	self assert: (interpreterToCompiler targetClass respondsTo: #basicPrimitiveTable).
	
]

{ #category : #tests }
DRInterpreterBuilderTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler := interpreterToCompiler primitives: Array empty.
	self assertEmpty: interpreterToCompiler primitives.
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	interpreterToCompiler targetClass: self interpreterClassForTest.
	self 
		assert: interpreterToCompiler primitives size 
		equals: 0.
	interpreterToCompiler addPrimitives: #(#primitiveAdd) .
	self
		assert: interpreterToCompiler primitives size 
		equals: interpreterToCompiler primitivesCount
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testGenerateSinglePrimitiveCompilationUnit [

	| primitives |
	interpreterToCompiler primitives: #(#primitiveAdd).
	primitives := interpreterToCompiler primitives.
	self
		assert: primitives notEmpty;
		assert: (primitives anyOne isKindOf: DRPrimitiveObject).
	self
		assertCollection: (primitives collect: #sourceSelector)
		hasSameElements: #(primitiveAdd)
]
