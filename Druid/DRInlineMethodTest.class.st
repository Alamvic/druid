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
DRInlineMethodTest >> testInlineMethod [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: cfg messageSends size equals: 2
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodWithMultiplePossibleTypes [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: cfg messageSends size equals: 4
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodWithNPossibleTypesMakesNMinus1TypeChecks [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: (cfg allConditionalJumps count: [:jump | jump condition isTypeOf]) equals: 1
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlineMethodWithNoTypeAnnotation [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithNoTypeAnnotation:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: cfg messageSends size equals: 1
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
