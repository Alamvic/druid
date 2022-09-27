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
DRBytecodeGeneratorTest >> generateDruidIRFor: methodSelector [

	| method cfg |
	method := DruidTestInterpreter lookupSelector: methodSelector.
	bytecodeGenerator numArgs: method numArgs.
	cfg := compilerCompiler generateDruidIRFor: method.
	compilerCompiler optimize: cfg.
	^ cfg
]

{ #category : #helpers }
DRBytecodeGeneratorTest >> generateMethodFor: aSelector [

	| cfg method |
	cfg := self generateDruidIRFor: aSelector.
	method := self generateMethodFromCFG: cfg withSelector: aSelector.
	^ method
]

{ #category : #'as yet unclassified' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector [
	
	DRRegisterCollapse new applyTo: aDRControlFlowGraph.
	bytecodeGenerator methodName: selector.
	bytecodeGenerator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ bytecodeGenerator targetAST
]

{ #category : #running }
DRBytecodeGeneratorTest >> setUp [
	super setUp.
	
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler interpreter: DruidTestInterpreter basicNew.
	compilerCompiler irGenerator: DRMethodIRGenerator new.
	
	bytecodeGenerator := DRBytecodeGenerator new.

]

{ #category : #tests }
DRBytecodeGeneratorTest >> testBasicControlFlow [

	| cfg method |
	cfg := DRControlFlowGraph new.

	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].

	method := self generateMethodFromCFG: cfg withSelector: #m.
	1 halt
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodInline [

	| method |
	method := self generateMethodFor: #basicInlineMethod.
	1 halt
]

{ #category : #tests }
DRBytecodeGeneratorTest >> testMethodWithArg [

	| method |
	method := self generateMethodFor: #lazyDouble:.
	1 halt
]
