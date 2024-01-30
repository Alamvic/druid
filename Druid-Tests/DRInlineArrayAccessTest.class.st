Class {
	#name : #DRInlineArrayAccessTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

{ #category : #asserting }
DRInlineArrayAccessTest >> assert: actualContext equalsContext: expectedContext [ 

	(actualContext isNil and: [ expectedContext isNil ])
		ifTrue: [ ^ self ].

	self assert: actualContext pc equals: expectedContext pc.
	self assert: actualContext stackPtr equals: expectedContext stackPtr.
	self assert: actualContext method equals: expectedContext method.
	self assert: actualContext closure equals: expectedContext closure.
	self assert: actualContext receiver equals: expectedContext receiver.

	1 to: actualContext stackPtr do: [ :i |
		self assert: (actualContext at: i) equals: (expectedContext at: i)
		 ]
]

{ #category : #tests }
DRInlineArrayAccessTest >> compileCFG: cfg selector: selector [

	| bytecodeGenerator |
	self optimizeCFG: cfg.
	bytecodeGenerator := DRBytecodeGenerator new.
	bytecodeGenerator numArgs: selector numArgs.
	bytecodeGenerator generateTargetASTFromIR: cfg methodName: selector.
	^ bytecodeGenerator compiledMethod
]

{ #category : #utilities }
DRInlineArrayAccessTest >> inlineAllMessages: aCollection in: aDRControlFlowGraph [ 
	
	[aDRControlFlowGraph messageSends anySatisfy: [ :e | aCollection includes: e selector ]]
		whileTrue: [ 
			aDRControlFlowGraph messageSends 
				detect: [ :e | aCollection includes: e selector ]
				ifFound: [ :e | e inline ]].
		
]

{ #category : #accessing }
DRInlineArrayAccessTest >> interpreterClass [ 

	^ DruidArrayAccessMockInterpreter
]

{ #category : #optimizing }
DRInlineArrayAccessTest >> optimizeCFG: aDRControlFlowGraph [ 

	aDRControlFlowGraph applyOptimisation: DRFrameInfoCleaner new.
	aDRControlFlowGraph applyOptimisation: DRBranchCollapse new.
	aDRControlFlowGraph applyOptimisation: DRSCCP new.
	aDRControlFlowGraph applyOptimisation: (DRCleanControlFlow then: DRCopyPropagation).
	aDRControlFlowGraph applyOptimisation: DRDeadCodeElimination new.

	DRLocalVariableInstructionScheluder new applyTo: aDRControlFlowGraph.

]

{ #category : #running }
DRInlineArrayAccessTest >> setUp [

	super setUp.
	compilerCompiler := DRMethodCompilerCompiler new.
	self configureInterpreter.

	compilerCompiler irGenerator: DRMethodIRGenerator new.

]

{ #category : #util }
DRInlineArrayAccessTest >> step: aMethod untilEnteringIn: aSelector receiver: aReceiver arguments: arguments [ 

	| rootCtx currentCtx |
	
	rootCtx := Context 
		sender: nil 
		receiver: aReceiver
		method: aMethod 
		arguments: { arguments }.

	currentCtx := rootCtx.

	[currentCtx method selector = aSelector ] whileFalse: [ currentCtx := currentCtx step].

	^ currentCtx
]

{ #category : #tests }
DRInlineArrayAccessTest >> testArrayAtInline [

	| cfg returns return |
	cfg := self generateDruidIRFor: #methodWithArrayUsingAt:.
	self inlineAllMessages: #(at:) in: cfg.

	self optimizeCFG: cfg.
	
	returns := cfg instructions select: [ :e | e isReturn ].
	
	self assert: returns size equals: 1.

	return := returns first.
	self assert: return operands first isLoadArrayElement.
	self assert: return operands first operands first isLoadArgument.
	self assert: return operands first operands second isValue.	
]

{ #category : #tests }
DRInlineArrayAccessTest >> testArrayAtInlineGeneratedMethod [

	| cfg m selector |

	selector := #methodWithArrayUsingAt:.
	cfg := self generateDruidIRFor: selector.

	self inlineAllMessages: #( #at: ) in: cfg.

	m := self compileCFG: cfg selector: selector.

	self
		assert: (m valueWithReceiver: nil arguments: { #( #a #b #c ) })
		equals: #a
]

{ #category : #tests }
DRInlineArrayAccessTest >> testArrayAtInlineGeneratedMethodDoesHasDeoptimizationInformation [

	| cfg m selector |

	selector := #methodWithArrayUsingAt:.
	cfg := self generateDruidIRFor: selector.

	self inlineAllMessages: #( #at: ) in: cfg.

	m := self compileCFG: cfg selector: selector.

	self assert: m deoptimizationInformation isNotNil

]

{ #category : #tests }
DRInlineArrayAccessTest >> testIteratingArrayUsingAt [

	| cfg loadArrayInstruction |
	cfg := self generateDruidIRFor: #methodIteratingArrayUsingAt:.
	self inlineAllMessages: #(size from:to:do: at:) in: cfg.

	self optimizeCFG: cfg.

	self assert: cfg allLoops size equals: 1.
	self assert: cfg allLoops first inductionVariable operands first variableName equals: #nextValue.
	self assert: (cfg instructions count: [ :e | e isLoadArrayElement ]) equals: 1.
	
	loadArrayInstruction := cfg instructions detect: [ :e | e isLoadArrayElement  ].

]

{ #category : #tests }
DRInlineArrayAccessTest >> testIteratingArrayUsingAtGeneratedMethodHasDeoptimizationInformation [

	| cfg m selector |

	selector := #methodIteratingArrayUsingAt:.
	cfg := self generateDruidIRFor: selector.
	
	self inlineAllMessages: #(size from:to:do: at:) in: cfg.

	m := self compileCFG: cfg selector: selector.
	
	self assert: m deoptimizationInformation isNotNil.
]

{ #category : #tests }
DRInlineArrayAccessTest >> testIteratingArrayUsingNotInlinedMethodGeneratedMethodHasDeoptimizationInformation [

	| cfg m selector |

	selector := #methodIteratingArrayUsingNonInlinedMethod:.
	cfg := self generateDruidIRFor: selector.
	
	self inlineAllMessages: #(size from:to:do: at:) in: cfg.

	m := self compileCFG: cfg selector: selector.
	
	self assert: m deoptimizationInformation isNotNil.
]

{ #category : #tests }
DRInlineArrayAccessTest >> testMethodInliningMessageSendMethodGeneratedMethodDeoptimizeStack [

	| cfg m selector optimizedCtx unoptimizedCtx receiver args |

	selector := #methodInliningMessageSend:.
	cfg := self generateDruidIRFor: selector.
	
	self inlineAllMessages: #(methodInlined:i:) in: cfg.

	m := self compileCFG: cfg selector: selector.

	receiver := DruidArrayAccessMockInterpreter new.
	args := { #(1 2 3) }.
	
	optimizedCtx := self step: m 
		untilEnteringIn: #methodNotToInline:i: 
		receiver: receiver
		arguments: args.

	unoptimizedCtx := self step: (self interpreterClass lookupSelector: selector)
		untilEnteringIn: #methodNotToInline:i: 
		receiver: receiver
		arguments: args.
			
	optimizedCtx deoptimizeSender.
	
	self assert: optimizedCtx equalsContext: unoptimizedCtx.
	self assert: optimizedCtx sender equalsContext: unoptimizedCtx sender.
	self assert: optimizedCtx sender sender equalsContext: unoptimizedCtx sender sender.
	self assert: optimizedCtx sender sender sender equalsContext: unoptimizedCtx sender sender sender.


]

{ #category : #tests }
DRInlineArrayAccessTest >> testMethodInliningMessageSendMethodGeneratedMethodHasDeoptimizationInformation [

	| cfg m selector |

	selector := #methodInliningMessageSend:.
	cfg := self generateDruidIRFor: selector.
	
	self inlineAllMessages: #(methodInlined:i:) in: cfg.

	m := self compileCFG: cfg selector: selector.
	
	self assert: m deoptimizationInformation isNotNil.
]

{ #category : #tests }
DRInlineArrayAccessTest >> testMethodWithUsingMultipleReturns [

	| cfg  |
	cfg := self generateDruidIRFor: #methodWithUsingMultipleReturns.
	self inlineAllMessages: #(multipleReturns:) in: cfg.

	self compileCFG: cfg selector: #methodWithUsingMultipleReturns.

]

{ #category : #tests }
DRInlineArrayAccessTest >> testMethodWithUsingMultipleReturnsDoesNotHaveDeoptimizationInformation [

	| cfg  m |
	cfg := self generateDruidIRFor: #methodWithUsingMultipleReturns.
	self inlineAllMessages: #(multipleReturns:) in: cfg.

	m := self compileCFG: cfg selector: #methodWithUsingMultipleReturns.

	self assert: m deoptimizationInformation isNil
]
