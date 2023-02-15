Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractMockTest,
	#category : #'Druid-Tests-CompilerCompiler'
}

{ #category : #initialization }
DRInterpreterToCompilerTest >> testCompilationUnitPopulateInitializePrimitiveTable [

	| interpreterCompiler |

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

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	self assert: interpreterCompiler primitives size equals: 2
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	self assert: interpreterCompiler primitives notEmpty
]
