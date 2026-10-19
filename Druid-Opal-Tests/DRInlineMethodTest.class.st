Class {
	#name : 'DRInlineMethodTest',
	#superclass : 'DRMethodOptimizationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'as yet unclassified' }
DRInlineMethodTest >> applyOptimisationOn: aCFG [ 

	aCFG applyOptimisation: optimisation
]

{ #category : 'asserting' }
DRInlineMethodTest >> assert: aDRBasicBlock isDominatedBy: dominatorBlock [ 

	self assert: (aDRBasicBlock isDominatedBy: dominatorBlock)
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testImplementorsTypeSystem [

	| cfg messageSend selector |

	optimisation typeSystem: DRImplementorsTypeSystem new.
	cfg := self generateDruidIRFor: #basicInlineMethod.
	selector := #doubleOf:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

	self assert: cfg messageSends isEmpty.
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodCopiesCalleeIntoCallerMethod [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	messageSend := cfg messageSends first.
	self applyOptimisationOn: cfg.

	self assert: cfg messageSends size equals: 2
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendFallbackAsLastBranch [

	| cfg messageSend inlinedCondition inlinedMessageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation:.
	messageSend := cfg messageSends first.
	self applyOptimisationOn: cfg.
	
	inlinedCondition := cfg allConditionalJumps first.
	inlinedMessageSend := cfg messageSends detect: [ :ms | ms originAST = messageSend originAST ].
	self assert: (inlinedCondition falseBranch successors anySatisfy: [:block | block includesInstruction: inlinedMessageSend]).
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendIfTypeIsUnknown [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotation:.
	messageSend := cfg messageSends unique.
	self applyOptimisationOn: cfg.
	
	self assert: (cfg messageSends anySatisfy: [ :ms | ms originAST = messageSend originAST ])
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testInlineMethodWithInlinedLoops [

	| cfg messageSend selector blockSend |
	cfg := self generateDruidIRFor: #doubleWhileWithBlock:.
	selector := #ifFalse:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

	blockSend := cfg messageSends unique.

	self assert: blockSend receiver isLoadArgument.
	self assert: blockSend receiver argNum equals: 1
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodWithMultiplePossibleTypesInlinesAllPossibleMethods [

	| cfg loads orderedCollection dictionary typeCheck phi |
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	self applyOptimisationOn: cfg.
	cfg applyOptimisation: (DRPhiSimplication then: DRDeadCodeElimination). "Clean code"

	self
		denyCollection: (cfg messageSends collect: [ :send | send selector ])
		includesAny: { #select: }.

	"Should create the type check for the guard"
	typeCheck := cfg allConditionalJumps unique.
	self assert: typeCheck condition isTypeOf.
	self assert: typeCheck operand1 isLoadArgument.
	self assert: typeCheck operand2 value isClassType.
	self assert: typeCheck operand2 value classType equals: OrderedCollection.

	"Get the collection result for both inlined cases"
	phi := cfg phiFunctions unique.
	loads := phi operands collect: [ :op | op simpleConstantFold ].
	self assert: loads size equals: 2.
	orderedCollection := loads first.
	dictionary := loads last.

	self assert: orderedCollection basicBlock isDominatedBy: typeCheck trueBranch.
	self assert: dictionary basicBlock isDominatedBy: typeCheck falseBranch.

	self assert: cfg lastBasicBlock endInstruction isReturn.
	self assert: cfg lastBasicBlock endInstruction operand1 simpleConstantFold equals: phi
]

{ #category : 'tests' }
DRInlineMethodTest >> testMessageSendsAreNotInlinedWhenCreatingCFGForMethod [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	self assert: cfg messageSends size equals: 1
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testMethodSendingIndirectClassGetToIt [

	| cfg  |

	cfg := self generateDruidIRFor: #methodInlineThenSendClass:.
	self applyOptimisationOn: cfg.

	self assert: cfg messageSends size > 1.
	self assert: cfg messageSends first selector equals: #class.	
]

{ #category : 'tests' }
DRInlineMethodTest >> testNotInlineMethodWithNoTypeAnnotation [

	| cfg |	
	cfg := self generateDruidIRFor: #methodWithNoTypeAnnotation:.
	self applyOptimisationOn: cfg.

	self assert: cfg messageSends size equals: 1
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testPragmaBasedTypeSystem [

	| cfg messageSend selector |
	cfg := self generateDruidIRFor: #methodWithArg:.
	
	selector := #select:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

	self assert: cfg messageSends size equals: 2.
	self
		denyCollection: (cfg messageSends collect: [ :m | m selector ])
		hasSameElements: selector
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_empty [

	| cfg messageSend selector |

	optimisation typeSystem: DRProfileBasedTypeSystem new.
	
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

	self assert: cfg messageSends unique selector equals: selector.

]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_monomorphic [

	| cfg messageSend selector typeSystem |
	
	optimisation typeSystem: (typeSystem := DRProfileBasedTypeSystem new).
	
	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	messageSend := cfg messageSends unique.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

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

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_polimorphic [

	| cfg messageSend selector typeSystem inlinedSend |

	optimisation typeSystem: (typeSystem := DRProfileBasedTypeSystem new).

	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	messageSend := cfg messageSends unique.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.
	typeSystem addReceiverInfo: Bag new inMessage: messageSend originAST.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

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
		equals: Bag.

	"Generic case"
	self assert: cfg messageSends second selector equals: selector.
	self
		assert: cfg messageSends second originAST methodNode selector
		equals: #methodCollectionSize:
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testUnknownTypeSystem [

	| cfg messageSend selector |
	optimisation typeSystem: DRUnknownTypeSystem new.

	cfg := self generateDruidIRFor: #methodWithArg:.
	selector := #select:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	self applyOptimisationOn: cfg.

	self assert: cfg messageSends unique selector equals: selector.

]
