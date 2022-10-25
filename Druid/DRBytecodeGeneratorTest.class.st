Class {
	#name : #DRBytecodeGeneratorTest,
	#superclass : #TestCase,
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator'
	],
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRBytecodeGeneratorTest >> generateDruidIRFor: method [

	| cfg |
	bytecodeGenerator numArgs: method numArgs.
	cfg := compilerCompiler generateDruidIRFor: method.
	1halt.
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
	
	"TODO: Sacar esto, iterar solamente las MandatoryInstructions"
	DRRegisterCollapse new applyTo: aDRControlFlowGraph.
	bytecodeGenerator := DRBytecodeGenerator new.
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

	method := self generateMethodFromCFG: cfg withSelector: #m.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 7
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodInline [

	| method result |
	method := self generateMethodForSelector: #basicInlineMethod.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 84
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

	| method |
	method := self generateMethodForSelector: #methodWithArg:.
	1 halt
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testOpal [

	compilerCompiler interpreter: OCOpalExamples new.

	OCOpalExamples methods
		reject: [ :opalMethod | opalMethod selector includesSubstring: 'Block' ]
		thenDo: [ :opalMethod | 
			| cfg method result |
			1 halt.
			cfg := self generateDruidIRFor: opalMethod.
			method := self generateMethodFromCFG: cfg withSelector: opalMethod selector.
			method methodClass: OCOpalExamples.
			result := method valueWithReceiver: OCOpalExamples new arguments: {  }.
			1 halt.
			self assert: result equals: (opalMethod valueWithReceiver: OCOpalExamples new arguments: {  }) ]
]
