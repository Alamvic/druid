Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractCompilerCollectorTest,
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
DRInterpreterToCompilerTest >> testAddBytecode [

	| bytecodeObj |
	interpreterToCompiler := DRInterpreterToCompiler
		                         fromInterpreterClass: DRBasicCogInterpreterArithmeticPrimitives.
	interpreterToCompiler
		doFailOnFirst;
		targetClass: self jitCompilerClassForTest;
		selectBytecodes: [ : cm | cm selector = #pushReceiverVariableBytecode ];
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler bytecodes notEmpty.

	bytecodeObj := interpreterToCompiler bytecodes first.
	self
		assert: bytecodeObj anyOne sourceSelector
		equals: #pushReceiverVariableBytecode.
		
	self
		assert: interpreterToCompiler bytecodeTable size
		equals: 16
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testAddEmptyInterpreterBytecode [

	interpreterToCompiler := DRInterpreterToCompiler
		                         fromInterpreterClass: DREmptyInterpreter.
	interpreterToCompiler
		doFailOnFirst;
		targetClass: self jitCompilerClassForTest;
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self assertEmpty: interpreterToCompiler bytecodes
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

	self assert: interpreterToCompiler primitiveTable notEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testErrorHandling [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter.
	interpreterToCompiler 
		targetClass: self jitCompilerClassForTest;
		selectPrimitives: [ : p | p selector = #primitiveDruidFail ];
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler errors notEmpty.
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DREmptyInterpreter.
	self assert: interpreterToCompiler primitives isEmpty
]
