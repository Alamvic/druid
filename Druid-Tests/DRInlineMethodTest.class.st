Class {
	#name : #DRInlineMethodTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #initialization }
DRInlineMethodTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new.
	self configureInterpreter.

	compilerCompiler irGenerator: DRMethodIRGenerator new.
	optimisation := DRInline new
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testImplementorsTypeSystem [

	| cfg messageSend selector |
	cfg := self generateDruidIRFor: #basicInlineMethod.
	selector := #lazyDouble:.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: DRImplementorsTypeSystem new.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Inline only one implementor"

	self assert: cfg messageSends isEmpty.
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

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testPragmaBasedTypeSystem [

	| cfg messageSend selector |
	cfg := self generateDruidIRFor: #methodWithArg:.
	selector := #select:.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: DRPragmaBasedTypeSystem new.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Inline only pragma"

	self assert: cfg messageSends size equals: 2.
	self
		denyCollection: (cfg messageSends collect: [ :m | m selector ])
		hasSameElements: selector
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_empty [

	| cfg messageSend selector |
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: DRProfileBasedTypeSystem new.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Empty profile - not inline"

	self assert: cfg messageSends unique selector equals: selector.

]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_monomorphic [

	| cfg messageSend selector typeSystem |
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	typeSystem := DRProfileBasedTypeSystem new.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: typeSystem.
	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Inline profile + generic gueard"

	"Guard"
	self assert: cfg allConditionalJumps size equals: 1.

	"Inlined case"
	cfg instructions
		select: [ :i | i isLoadInstanceVariable ]
		thenDo: [ :i |
			self assert:
				({ #firstIndex. #lastIndex } includes: i variableName) ].

	"Generic case"
	self assert: cfg messageSends unique selector equals: selector
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_polimorphic [

	| cfg messageSend selector typeSystem inlinedSend |
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	typeSystem := DRProfileBasedTypeSystem new.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: typeSystem.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.
	typeSystem addReceiverInfo: Array2D new inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Inline profile + generic gueard"

	"Guard"
	self assert: cfg allConditionalJumps size equals: 2.

	"Inlined case"
	cfg instructions
		select: [ :i | i isLoadInstanceVariable ]
		thenDo: [ :i |
			i variableName = #contents
				ifTrue: [ "Array2D inline" inlinedSend := i users unique ]
				ifFalse: [ "OrderedCollection inline"
					self assert:
						({ #firstIndex. #lastIndex } includes: i variableName) ] ].

	self
		assert: inlinedSend originAST methodNode methodClass
		equals: Array2D.

	"Generic case"
	self assert: cfg messageSends second selector equals: selector.
	self
		assert: cfg messageSends second originAST methodNode selector
		equals: #methodCollectionSize:
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testUnknownTypeSystem [

	| cfg messageSend selector |
	cfg := self generateDruidIRFor: #methodWithArg:.
	selector := #select:.
	messageSend := cfg messageSends unique.
	messageSend typeSystem: DRUnknownTypeSystem new.

	self assert: messageSend selector equals: selector.

	messageSend inline. "Not inlining"

	self assert: cfg messageSends unique selector equals: selector.

]
