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
DRInlineMethodTest >> testInlineMethodCopiesCalleeIntoCallerMethod [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: cfg messageSends size equals: 2
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendFallbackAsLastBranch [

	| cfg messageSend inlinedCondition inlinedMessageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation:.
	messageSend := cfg messageSends first.
	messageSend inline.

	inlinedCondition := cfg allConditionalJumps first.
	inlinedMessageSend := cfg messageSends detect: [ :ms | ms originAST = messageSend originAST ].
	self assert: (inlinedCondition falseBranch successors anySatisfy: [:block | block includesInstruction: inlinedMessageSend]).
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendIfTypeIsUnknown [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotation:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: (cfg messageSends anySatisfy: [ :ms | ms originAST = messageSend originAST ])
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodWithMultiplePossibleTypesInlinesAllPossibleMethods [

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
DRInlineMethodTest >> testMessageSendsAreNotInlinedWhenCreatingCFGForMethod [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	self assert: cfg messageSends size equals: 1
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlineMethodWithNoTypeAnnotation [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithNoTypeAnnotation:.
	messageSend := cfg messageSends first.
	messageSend inline.

	self assert: cfg messageSends size equals: 1
]
