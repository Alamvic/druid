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

	^ testingEnvironment 
		at: self interpreterClassNameForTest 
		ifAbsent: [ 
			Smalltalk image classInstaller make: [ : builder |
				builder
					name: self interpreterClassNameForTest;
					superclass: self interpreterSuperclassForTest;
					category: self interpreterClassNameForTest ]. ]
]

{ #category : #tests }
DRInterpreterBuilderTest >> interpreterClassNameForTest [

	^ #DRNewInterpreterForTest
]

{ #category : #running }
DRInterpreterBuilderTest >> interpreterSuperclassForTest [

	^ DRBasicCogInterpreterArithmeticPrimitives
]

{ #category : #running }
DRInterpreterBuilderTest >> setUp [

	super setUp.
	interpreterToCompiler := DRInterpreterBuilderToCompiler new
		targetSuperclass: self interpreterSuperclassForTest;
		yourself.

]

{ #category : #running }
DRInterpreterBuilderTest >> tearDown [

	interpreterToCompiler targetClass ifNotNil: [ : tc | 
		(RGContainer packageOfClass: tc) realPackage removeFromSystem ].

	SystemOrganizer default removeSystemCategory: druidInterpreter class name.
	testingEnvironment removeClassNamed: druidInterpreter class name.
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
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testCompilePrimitive [

	| primitives |
	
	interpreterToCompiler addPrimitives: #(#primitiveAdd).
	primitives := interpreterToCompiler primitives.
	self assert: primitives notEmpty.
	self
		assert: primitives anyOne sourceSelector
		equals: #primitiveAdd.
	interpreterToCompiler compileAll.
	self
		assertCollection: interpreterToCompiler interpreterClass primitiveTable
		hasSameElements: #(0 #primitiveAdd #primitiveFail)
]

{ #category : #tests }
DRInterpreterBuilderTest >> testCompilePrimitiveTable [

	interpreterToCompiler buildIRAndCompileIn: self interpreterClassNameForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterBuilderTest >> testConfigureInterpreterClass [

	interpreterToCompiler targetClass: self interpreterClassForTest.

	self 
		assertCollection: interpreterToCompiler targetClass basicPrimitiveTable 
		hasSameElements: #(0 #primitiveAdd #primitiveSubtract #primitiveLessThan #primitiveGreaterThan #primitiveLessOrEqual #primitiveGreaterOrEqual #primitiveEqual #primitiveNotEqual #primitiveMultiply #primitiveDivide).
	
	interpreterToCompiler 
		addPrimitives: #(#primitiveAdd);
		compileAll.
	self assert: interpreterToCompiler primitives notEmpty.
	self assert: interpreterToCompiler primitives anyOne sourceSelector equals: #primitiveAdd
	
]

{ #category : #initialization }
DRInterpreterBuilderTest >> testCountPrimimitives [

	interpreterToCompiler targetClass: self interpreterClassForTest.
	self 
		assert: interpreterToCompiler primitives size 
		equals: 0.
	interpreterToCompiler addPrimitives: #(#primitiveAdd) .
	self
		assert: interpreterToCompiler primitives size 
		equals: 1.
	self
		assert:  interpreterToCompiler primitivesCount
		equals: 10.
]

{ #category : #tests }
DRInterpreterBuilderTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler primitives: Array empty.
	self assertEmpty: interpreterToCompiler primitives.
]
