Class {
	#name : #DRPrimitiveCompilerCompilerCollectorTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'testingEnvironment'
	],
	#category : #'Druid-Tests'
}

{ #category : #accessing }
DRPrimitiveCompilerCompilerCollectorTest >> interpreterWith: aCollection [

	^ self newEmptyInterpreter primitives: aCollection
]

{ #category : #accessing }
DRPrimitiveCompilerCompilerCollectorTest >> newEmptyInterpreter [

	^ DRDummyInterpreter new
]

{ #category : #running }
DRPrimitiveCompilerCompilerCollectorTest >> setUp [

	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : #running }
DRPrimitiveCompilerCompilerCollectorTest >> tearDown [

	testingEnvironment removeClassNamed: self testingJITCompilerClass.
	super tearDown.
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitCollectsPrimitives [

	| interpreterCompiler interpreterBuilder |

	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler generateIR hasPrimitives
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitGeneratesJIT [

	| interpreterCompiler interpreterBuilder |
	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateIRAndCompileIn: self testingJITCompilerClass.

	self shouldnt: [ self class environment at: self testingJITCompilerClass ] raise: KeyNotFound.
	self assert: ((self class environment at: self testingJITCompilerClass) respondsTo: #initializePrimitiveTable)
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testCompilationUnitModelGeneratesJIT [

	| interpreterCompiler interpreterBuilder |
	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	interpreterCompiler generateBuildModelAndCompileIn: self testingJITCompilerClass.

	self shouldnt: [ self class environment at: self testingJITCompilerClass ] raise: KeyNotFound.
	self assert: ((self class environment at: self testingJITCompilerClass) respondsTo: #initializePrimitiveTable)
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testGenerateEmptyCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: Array empty).
	self assert: interpreterCompiler primitives isEmpty
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testGenerateMultiplePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd #primitiveEqual)).
	self assert: interpreterCompiler primitives size equals: 2
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testGenerateMultiplePrimitiveObjectCompilationUnit [

	| interpreterCompiler interpreterBuilder |

	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler primitives size equals: interpreterBuilder primitivesCount
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testGenerateSinglePrimitiveCompilationUnit [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: (self interpreterWith: #(#primitiveAdd)).
	self assert: interpreterCompiler primitives notEmpty
]

{ #category : #initialization }
DRPrimitiveCompilerCompilerCollectorTest >> testObtainCompilationUnit [

	| interpreterCompiler interpreterBuilder |

	interpreterBuilder := DRInterpreterBuilder new.
	interpreterCompiler := DRInterpreterCompiler fromInterpreter: interpreterBuilder.
	self assert: interpreterCompiler generateIR notNil
]

{ #category : #running }
DRPrimitiveCompilerCompilerCollectorTest >> testingJITCompilerClass [

	^ #MyJITCompilerClass
]
