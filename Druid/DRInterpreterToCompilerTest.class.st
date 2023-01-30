Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
]

{ #category : #initialization }
DRInterpreterToCompilerTest >> testGenerateClass [

	| interpreterCompiler |

	interpreterCompiler := DRInterpreterCompiler fromInterpreter: DREmptyInterpreter.
	self assert: interpreterCompiler primitives isEmpty.
	

]
