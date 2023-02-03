Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Druid-Tests'
}

{ #category : #accessing }
DRInterpreterToCompilerTest >> interpreterWith: aCollection [

	^ self newEmptyInterpreter primitives: aCollection
]

{ #category : #accessing }
DRInterpreterToCompilerTest >> newEmptyInterpreter [

	^ DRDummyInterpreter new
]

{ #category : #running }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : #running }
DRInterpreterToCompilerTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass.
	super tearDown
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	| interpreterCompiler interpreterBuilder |
	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self testingJITCompilerClass.

	self assert: interpreterCompiler initPrimitiveTable notEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter:
		(self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	self assert: interpreterCompiler primitives size equals: 2
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	| interpreterCompiler interpreterBuilder |

	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	self assert: interpreterCompiler primitives notEmpty
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testingJITCompilerClass [

	^ #MyJITCompilerClass
]
