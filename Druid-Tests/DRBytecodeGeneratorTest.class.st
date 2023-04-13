Class {
	#name : #DRBytecodeGeneratorTest,
	#superclass : #TestCase,
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator',
		'typeSystem'
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
	cfg messageSends do: [ :send | send typeSystem: typeSystem ].
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

	"TODO: Improve transformation Register to Stack"
	DRRegisterCollapse new applyTo: aDRControlFlowGraph.
	
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
	typeSystem := DRPragmaBasedTypeSystem new
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
DRBytecodeGeneratorTest >> testMethodRecompilationWithProfile [

	| method result typeEntries cfg selector |
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	typeSystem := DRProfileBasedTypeSystem reset.
	selector := #methodCollectionSize:.
	method := self generateMethodForSelector: selector.

	self assert: method size equals: 53. "Small"

	"First execution"
	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	"Profiled type"
	typeEntries := typeSystem profileTable associations.
	self assert: typeEntries size equals: 1.
	self
		assert: typeEntries first key
		equals: (RBVariableNode named: 'arg1').
	self
		assertCollection: typeEntries first value
		hasSameElements: { (DRClassType for: OrderedCollection) }.

	"Recompile"
	compilerCompiler := DRMethodCompilerCompiler new.
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size equals: 121. "bigger"

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4.

	"New profiled type"
	typeEntries := typeSystem profileTable associations.
	self assert: typeEntries size equals: 1.
	self
		assert: typeEntries first key
		equals: (RBVariableNode named: 'arg1').
	self assertCollection: typeEntries first value hasSameElements: {
			(DRClassType for: OrderedCollection).
			(DRClassType for: Array2D) }.

	"Recompile again"
	compilerCompiler := DRMethodCompilerCompiler new.
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size equals: 250. "bigger then before"
	
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
