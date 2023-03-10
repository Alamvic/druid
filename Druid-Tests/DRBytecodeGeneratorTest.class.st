Class {
	#name : #DRBytecodeGeneratorTest,
	#superclass : #TestCase,
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator'
	],
	#category : #'Druid-Tests'
}

{ #category : #assertion }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method [

	| result expectedResult |
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: {  }.
	expectedResult := method valueWithReceiver: compilerCompiler interpreter arguments: {  }.
	self assert: result equals: expectedResult
]

{ #category : #assertion }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass [

	| compiledMethod expectedMethod |
	expectedMethod := aClass >> aSelector.
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : #tests }
DRBytecodeGeneratorTest >> compileSelector: selector fromClass: interpreterClass [

	| method interpreter cfg |
	method := interpreterClass >> selector.
	interpreter := interpreterClass basicNew.
	compilerCompiler interpreter: interpreter.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.
	method methodClass: interpreterClass.
	^ method
]

{ #category : #tests }
DRBytecodeGeneratorTest >> generateDruidIRFor: method [

	| cfg |
	cfg := compilerCompiler generateDruidIRFor: method.
	compilerCompiler optimize: cfg.
	^ cfg
]

{ #category : #tests }
DRBytecodeGeneratorTest >> generateDruidIRForSelector: selector [

	^ self generateDruidIRFor: (DruidTestInterpreter lookupSelector: selector)
]

{ #category : #helpers }
DRBytecodeGeneratorTest >> generateMethodForSelector: aSelector [

	| cfg method |
	cfg := self generateDruidIRForSelector: aSelector.
	method := self generateMethodFromCFG: cfg withSelector: aSelector.
	^ method
]

{ #category : #'as yet unclassified' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector [

	^ self
		  generateMethodFromCFG: aDRControlFlowGraph
		  withSelector: selector
		  numArgs: compilerCompiler irGenerator numberOfArguments
]

{ #category : #'as yet unclassified' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector numArgs: numArgs [

	"TODO: Sacar esto, iterar solamente las MandatoryInstructions"
	DRRegisterCollapse new applyTo: aDRControlFlowGraph.
	
	bytecodeGenerator := DRBytecodeGenerator new.
	bytecodeGenerator numArgs: numArgs.
	bytecodeGenerator methodName: selector.
	bytecodeGenerator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ bytecodeGenerator targetAST
]

{ #category : #running }
DRBytecodeGeneratorTest >> setUp [
	super setUp.
	
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler interpreter: DruidTestInterpreter basicNew.
	
	bytecodeGenerator := DRBytecodeGenerator new.

]

{ #category : #tests }
DRBytecodeGeneratorTest >> testBasicControlFlow [

	| cfg method result |
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].

	method := self generateMethodFromCFG: cfg withSelector: #m numArgs: 0.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 7
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodEmpty [

	self assertCompilationFor: #exampleEmptyMethod fromClass: OCOpalExamples
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodInline [

	self assertCompilationFor: #basicInlineMethod fromClass: DruidTestInterpreter
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DruidTestInterpreter
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodWithArg [

	| method result |
	method := self generateMethodForSelector: #lazyDouble:.
	result := method valueWithReceiver: nil arguments: { 12 }.

	self assert: result equals: 24
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 0 }.

	self assert: result equals: 42
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn2 [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 1 }.

	self assert: result equals: 57
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodWithTypeCheck [

	| method result |
	method := self generateMethodForSelector: #methodCollectionSize:.

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testOpalBooleanMethods [

	self skip. "TO FIX"
	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocol = #'examples - andor' ]
		thenDo: [ :opalMethod | self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testOpalConditionalMethods [

	self skip. "TO FIX"
	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocol = #'examples - conditionals' ]
		thenDo: [ :opalMethod | self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testOpalSimpleMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocol = #'examples - simple' ]
		thenDo: [ :opalMethod | self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]
