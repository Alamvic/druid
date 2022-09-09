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

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
1halt.
	messageSend := cfg messageSends first.
	messageSend methodNode: ((OrderedCollection lookupSelector: messageSend originAST selector) ast) .
	messageSend inline.
	self assert: cfg messageSends size equals: 2
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlinedMethodWithoutArgs [

	| cfg |
	cfg := self generateDruidIRFor: #methodWithoutArgs.
1halt.
	self assert: cfg messageSends size equals: 1
]
