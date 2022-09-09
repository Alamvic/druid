Class {
	#name : #DRInlineMethodTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests'
}

{ #category : #initialization }
DRInlineMethodTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new.
	self configureInterpreter.

	compilerCompiler irGenerator: DRMethodIRGenerator new.
	optimisation := DRInline new
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlinedMethodWithArgs [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithArg:.

	self assert: cfg messageSends size equals: 1
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlinedMethodWithoutArgs [

	| cfg |
	cfg := self generateDruidIRFor: #methodWithoutArgs.

	self assert: cfg messageSends size equals: 1
]
