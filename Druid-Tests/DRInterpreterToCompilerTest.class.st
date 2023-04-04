Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractMockTest,
	#instVars : [
		'interpreterToCompiler'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #running }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testAddPrimitive [

	interpreterToCompiler := DRInterpreterToCompiler
		                         fromInterpreterClass: DruidTestInterpreter.
	interpreterToCompiler
		doFailOnFirst;
		targetClass: self jitCompilerClassForTest;
		selectPrimitives: [ :p | p selector = #primitiveAdd ];
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self
		assert: interpreterToCompiler primitives first sourceSelector
		equals: #primitiveAdd
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testAddPrimitives [

	| primitiveSet |
	primitiveSet := #(#primitiveAdd #primitiveSubtract #primitiveLessThan #primitiveGreaterThan #primitiveLessOrEqual #primitiveGreaterOrEqual #primitiveEqual #primitiveNotEqual #primitiveMultiply #primitiveDivide).
	
	interpreterToCompiler buildAndCompileIn: self jitCompilerClassNameForTest.
	self
		assertCollection: (interpreterToCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet.

	self
		assert: (interpreterToCompiler primitives allSatisfy: [ : p | p isKindOf: DRJITPrimitiveObject ])
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testCompilePrimitiveTable [

	interpreterToCompiler buildAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler initPrimitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testErrorHandling [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter.
	interpreterToCompiler 
		targetClass: self jitCompilerClassForTest;
		selectPrimitives: [ : p | p selector = #primitiveDruidFail ];
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler failedPrimitives notEmpty.
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DREmptyInterpreter.
	self assert: interpreterToCompiler primitives isEmpty
]
