Class {
	#name : 'DRInlineMethodTest',
	#superclass : 'DRMethodOptimizationTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'asserting' }
DRInlineMethodTest >> assert: aDRBasicBlock isDominatedBy: dominatorBlock [ 

	self assert: (aDRBasicBlock isDominatedBy: dominatorBlock)
]

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testImplementorsTypeSystem [

	| cfg messageSend selector |

	compilerCompiler irGenerator typeSystem: DRImplementorsTypeSystem new.
	cfg := self generateDruidIRFor: #basicInlineMethod.
	selector := #doubleOf:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new. "Inline only one implementor"

	self assert: cfg messageSends isEmpty.
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodCopiesCalleeIntoCallerMethod [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithArg:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends size equals: 2
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendFallbackAsLastBranch [

	| cfg messageSend inlinedCondition inlinedMessageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotationBeforeConcreteTypeAnnotation:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.
	
	inlinedCondition := cfg allConditionalJumps first.
	inlinedMessageSend := cfg messageSends detect: [ :ms | ms originAST = messageSend originAST ].
	self assert: (inlinedCondition falseBranch successors anySatisfy: [:block | block includesInstruction: inlinedMessageSend]).
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodKeepsMessageSendIfTypeIsUnknown [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithUnknownTypeAnnotation:.
	messageSend := cfg messageSends unique.

	cfg applyOptimisation: DRInline new.
	
	self assert: (cfg messageSends anySatisfy: [ :ms | ms originAST = messageSend originAST ])
]

{ #category : 'tests' }
DRInlineMethodTest >> testInlineMethodWithMultiplePossibleTypesInlinesAllPossibleMethods [

	| cfg loads orderedCollection dictionary typeCheck phi |
	cfg := self generateDruidIRFor: #methodWithMultipleTypeAnnotations:.
	cfg applyOptimisation: DRInline new.
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

	cfg applyOptimisation: DRInline new. "Inline only one implementor"

	self assert: cfg messageSends size > 1.
	self assert: cfg messageSends first selector equals: #class.	
]

{ #category : 'tests' }
DRInlineMethodTest >> testNotInlineMethodWithNoTypeAnnotation [

	| cfg messageSend |	
	cfg := self generateDruidIRFor: #methodWithNoTypeAnnotation:.
	messageSend := cfg messageSends first.
	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends size equals: 1
]

{ #category : 'tests - type systems' }
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

{ #category : 'tests - type systems' }
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

{ #category : 'tests - type systems' }
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

{ #category : 'tests - type systems' }
DRInlineMethodTest >> testProfileBasedTypeSystem_polimorphic [

	| cfg messageSend selector typeSystem inlinedSend |

	compilerCompiler irGenerator typeSystem: (typeSystem := DRProfileBasedTypeSystem new).

	cfg := self generateDruidIRFor: #methodCollectionSize:.
	selector := #size.

	messageSend := cfg messageSends unique.

	typeSystem
		addReceiverInfo: OrderedCollection new
		inMessage: messageSend originAST.
	typeSystem addReceiverInfo: Bag new inMessage: messageSend originAST.

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
	compilerCompiler irGenerator typeSystem: DRUnknownTypeSystem new.

	cfg := self generateDruidIRFor: #methodWithArg:.
	selector := #select:.
	messageSend := cfg messageSends unique.

	self assert: messageSend selector equals: selector.

	cfg applyOptimisation: DRInline new.

	self assert: cfg messageSends unique selector equals: selector.

]
