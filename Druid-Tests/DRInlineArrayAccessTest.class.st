Class {
	#name : #DRInlineArrayAccessTest,
	#superclass : #DROptimisationTest,
	#category : #'Druid-Tests-Optimizations'
}

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
DRInlineArrayAccessTest >> testArrayAtInlineGeneratedMethodDoesNotHaveDeoptimizationInformation [

	| cfg m selector |

	selector := #methodWithArrayUsingAt:.
	cfg := self generateDruidIRFor: selector.

	self inlineAllMessages: #( #at: ) in: cfg.

	m := self compileCFG: cfg selector: selector.

	self assert: m deoptimizationInformation equals: nil

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
