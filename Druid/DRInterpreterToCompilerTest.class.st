Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #accessing }
DRInterpreterToCompilerTest >> interpreterWith: aCollection [ 

	^ DRDummyInterpreter new primitives: aCollection
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateEmptyCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty.

]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateSinglePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	self assert: interpreterCompiler primitives notEmpty.

]
