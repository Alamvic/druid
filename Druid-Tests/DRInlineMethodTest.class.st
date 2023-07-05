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

	compilerCompiler irGenerator typeSystem: DRImplementorsTypeSystem new.
	cfg := self generateDruidIRFor: #basicInlineMethod.
	selector := #lazyDouble:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new. "Inline only one implementor"

	self assert: cfg messageSends isEmpty.
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodCopiesCalleeIntoCallerMethod [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends size equals: 2
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendFallbackAsLastBranch [

	| cfg messageSend inlinedCondition inlinedMessageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.
	
	inlinedCondition := cfg allConditionalJumps first.
	inlinedMessageSend := cfg messageSends detect: [ :ms | ms originAST = messageSend originAST ].
	self assert: (inlinedCondition falseBranch successors anySatisfy: [:block | block includesInstruction: inlinedMessageSend]).
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendIfTypeIsUnknown [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotation:.
	messageSend := cfg messageSends unique.

	cfg applyOptimisation: DRInline new.
	
	self assert: (cfg messageSends anySatisfy: [ :ms | ms originAST = messageSend originAST ])
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodWithMultiplePossibleTypesInlinesAllPossibleMethods [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	cfg applyOptimisation: DRInline new.
	
	self assert: cfg messageSends size equals: 4
]

{ #category : #tests }
DRInlineMethodTest >> testInlineMethodWithNPossibleTypesMakesNMinus1TypeChecks [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.

	cfg applyOptimisation: DRInline new.
	
	self assert: (cfg allConditionalJumps count: [:jump | jump condition isTypeOf]) equals: 1
]

{ #category : #tests }
DRInlineMethodTest >> testMessageSendsAreNotInlinedWhenCreatingCFGForMethod [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	self assert: cfg messageSends size equals: 1
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testMethodSendingIndirectClassGetToIt [

	| cfg  |

	cfg := self generateDruidIRFor: #methodSendingClass:.

	cfg applyOptimisation: DRInline new. "Inline only one implementor"

	self assert: cfg messageSends size equals: 1.
	self assert: cfg messageSends unique selector equals: #class.	
]

{ #category : #tests }
DRInlineMethodTest >> testNotInlineMethodWithNoTypeAnnotation [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithNoTypeAnnotation:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends size equals: 1
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testPragmaBasedTypeSystem [

	| cfg messageSend selector |
	compilerCompiler irGenerator typeSystem: DRPragmaBasedTypeSystem new.
	
	cfg := self generateDruidIRFor: #methodWithArg:.
	
	selector := #select:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new. "Inline only pragma"

	self assert: cfg messageSends size equals: 2.
	self
		denyCollection: (cfg messageSends collect: [ :m | m selector ])
		hasSameElements: selector
]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_empty [

	| cfg messageSend selector |

	compilerCompiler irGenerator typeSystem: DRProfileBasedTypeSystem new.
	
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new.  "Empty profile - not inline"

	self assert: cfg messageSends unique selector equals: selector.

]

{ #category : #'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_monomorphic [

	| cfg messageSend selector typeSystem |
	
	compilerCompiler irGenerator typeSystem: (typeSystem := DRProfileBasedTypeSystem new).
	
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	messageSend := cfg messageSends unique.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new. "Inline profile + generic gueard"

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

	compilerCompiler irGenerator typeSystem: (typeSystem := DRProfileBasedTypeSystem new).

	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	messageSend := cfg messageSends unique.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.
	typeSystem addReceiverInfo: Array2D new inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new. "Inline profile + generic gueard"

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
	compilerCompiler irGenerator typeSystem: DRUnknownTypeSystem new.

	cfg := self generateDruidIRFor: #methodWithArg:.
	selector := #select:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends unique selector equals: selector.

]
