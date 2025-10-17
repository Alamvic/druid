Class {
	#name : 'DRBytecodeGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator',
		'typeSystem'
	],
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal',
	#tag : 'Tests'
}

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method [

	self assert: compiledMethod equalsResult: method arguments: {  }
]

{ #category : 'asserting' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method arguments: arguments [

	| result expectedResult |

	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: arguments.
	expectedResult := method valueWithReceiver: compilerCompiler interpreter arguments: arguments.

	expectedResult isClosure ifTrue: [ 
		^ self assertClosure: result equals: expectedResult ].

	self assert: result equals: expectedResult
]

{ #category : 'asserting' }
DRBytecodeGeneratorTest >> assertClosure: result equals: expected [

	| blockResult blockExpected |

	self assert: result isClosure.
	self assert: expected isClosure.	
	self assert: result argumentCount equals: expected argumentCount.
	
	"Try some value?"
	blockResult := result valueWithArguments: (Array ofSize: result argumentCount).
	blockExpected := result valueWithArguments: (Array ofSize: result argumentCount).
	self assert: blockResult equals: blockExpected
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass [

	| compiledMethod expectedMethod |
	
	compilerCompiler configureForCompilerClass: nil.
	expectedMethod := aClass >> aSelector.
	"Ignore the methods with arguments"
	expectedMethod numArgs = 0 ifFalse: [ ^ self ].
	
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass checkByteCode: byteCode [

	| compiledMethod expectedMethod |
	self setUp.
	compilerCompiler configureForCompilerClass: nil.
	
	expectedMethod := aClass >> aSelector.
	"Ignore the methods with arguments"
	expectedMethod numArgs = 0 ifFalse: [ ^ self ].
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: (self checkByteCode: byteCode in: compiledMethod ir).
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationNoInlineFor: aSelector fromClass: aClass [

	| compiledMethod expectedMethod |
	self setUp.
	compilerCompiler configureForCompilerClassWithoutInline: nil.
	
	expectedMethod := aClass >> aSelector.
	"Ignore the methods with arguments"
	expectedMethod numArgs = 0 ifFalse: [ ^ self ].
	
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationNoInlineFor: aSelector fromClass: aClass arguments: args [

	| compiledMethod expectedMethod |
	compilerCompiler configureForCompilerClassWithoutInline: nil.

	expectedMethod := aClass >> aSelector.

	compiledMethod := self compileSelector: aSelector fromClass: aClass.

	self
		assert: compiledMethod
		equalsResult: expectedMethod
		arguments: args
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> checkByteCode: byteCode in: compiledMethodIR [
	"check if the provided bytecode exist in the compiledMethod IR"
	
	^ compiledMethodIR startSequence sequence anySatisfy: [ :each | each printString includesAll: byteCode]
]

{ #category : 'tests' }
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

{ #category : 'tests' }
DRBytecodeGeneratorTest >> generateDruidIRFor: method [

	| cfg |
	cfg := compilerCompiler generateDruidIRFor: method.
	
	compilerCompiler optimize: cfg.
	
	^ cfg
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> generateDruidIRForSelector: selector [

	^ self generateDruidIRFor: (DrOpalExamples lookupSelector: selector)
]

{ #category : 'helpers' }
DRBytecodeGeneratorTest >> generateMethodForSelector: aSelector [

	| cfg method |
	cfg := self generateDruidIRForSelector: aSelector.
	method := self generateMethodFromCFG: cfg withSelector: aSelector.
	^ method
]

{ #category : 'utilities' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector [

	^ self
		  generateMethodFromCFG: aDRControlFlowGraph
		  withSelector: selector
		  numArgs: compilerCompiler irGenerator numberOfArguments
]

{ #category : 'utilities' }
DRBytecodeGeneratorTest >> generateMethodFromCFG: aDRControlFlowGraph withSelector: selector numArgs: numArgs [
	
	aDRControlFlowGraph applyOptimisation: DRFrameInfoCleaner new.
	aDRControlFlowGraph applyOptimisation: DRBranchCollapse new.
	
	aDRControlFlowGraph applyOptimisation: DRPhiSimplication new.
	aDRControlFlowGraph applyOptimisation: (DRSCCP then: DRDeadCodeElimination).
	
	DRLocalVariableInstructionScheluder new applyTo: aDRControlFlowGraph.

	bytecodeGenerator numArgs: numArgs.
	bytecodeGenerator methodName: selector.
	bytecodeGenerator generateTargetASTFromIR: aDRControlFlowGraph methodName: selector.
	^ bytecodeGenerator targetAST
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> setUp [
	super setUp.
	
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler interpreter: DrOpalExamples basicNew.
	
	bytecodeGenerator := DRBytecodeGenerator new
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> tearDown [

	| aPackage |
	aPackage := RPackageOrganizer default packageNamed: 'DrOpal-Extensions' ifAbsent: nil.
	aPackage ifNotNil: [ aPackage removeFromSystem ].
	
	super tearDown.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testBasicControlFlow [

	| cfg method result |
	cfg := DRMethodControlFlowGraph new.
	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].
	cfg scope: DRScope new.
	method := self generateMethodFromCFG: cfg withSelector: #m numArgs: 0.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 7
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodEmpty [

	self assertCompilationFor: #exampleEmptyMethod fromClass: OCOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInline [

	self assertCompilationFor: #basicInlineMethod fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineObjectAccessInstVar [

	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DrOpalExamples checkByteCode: 'send: #instVarAt:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSettingInstVar fromClass: DrOpalExamples checkByteCode: 'send: #instVarAt:put:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineSelfAccessInstVar [

	self assertCompilationFor: #basicInlineMethodSelfAccesInstVar fromClass: DrOpalExamples checkByteCode: 'pushInstVar:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineSelfSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSelfSettingInstVar fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarOnReturn fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarWithUsers fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineToDo [

	self assertCompilationFor: #methodWithToDo fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineToDoInner [

	self assertCompilationFor: #methodWithTwoToDos fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodReadingClassVariable [

	self assertCompilationFor: #methodReadingClassVariable fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithArg [

	| method result |
	compilerCompiler configureForCompilerClass: nil.
	method := self generateMethodForSelector: #lazyDouble:.
	result := method valueWithReceiver: nil arguments: { 12 }.

	self assert: result equals: 24
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivated [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivated
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivatedArg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivatedArg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWithArg fromClass: DrOpalExamples arguments: {}.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockArgWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodArgWithBlockWithArg: fromClass: DrOpalExamples arguments: { nil }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	self assertCompilationNoInlineFor: #methodBlockConstant fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled1Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull1Arg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled2Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull2Arg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	self assertCompilationNoInlineFor: #methodEmptyBlock fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockLocalAndNonLocalReturn fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWrite [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWrite fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWriteWithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWriteWithArg: fromClass: DrOpalExamples arguments: { 5 }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg: fromClass: DrOpalExamples arguments: {1}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg2 [

	self
		assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg2:
		fromClass: DrOpalExamples
		arguments: { 1 }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp2 fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2Simple [
	
	self assertCompilationNoInlineFor: #methodBlockNested2Simple fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg2 fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp2 fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockNonLocalReturn fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingArgVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingArgVariable: fromClass: DrOpalExamples arguments: { 0 }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingVar [

	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingOuterVariable fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockTemp [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlock
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockWritingVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWritingOuterVariable fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithDynamicArray [

	self assertCompilationFor: #methodWithDynamicArray fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn [

	| method result |
	compilerCompiler configureForCompilerClass: nil.
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 0 }.

	self assert: result equals: 42
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn2 [

	| method result |
	compilerCompiler configureForCompilerClass: nil.
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 1 }.

	self assert: result equals: 57
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithTypeCheck [

	| method result |
	
	compilerCompiler configureForCompilerClass: nil.
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

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWritingClassVariable [

	self assertCompilationFor: #methodWritingClassVariable fromClass: DrOpalExamples
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalBooleanMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - andor' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalConditionalMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - conditionals' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalLiteralMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - literals' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalMiscMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - misc' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalSimpleMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - simple' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testOpalVariablesMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - variables' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethod [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethod.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodReturnArg [
	
	| compiledMethod expectedMethod aSelector result |

	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodReturnArg: .
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 5.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWith1arg [
	
	| compiledMethod expectedMethod aSelector result|
	
	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodWith1Arg:.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 6.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWith2arg [
	
	| compiledMethod expectedMethod aSelector result |
	
	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodWith1Arg:arg2:.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 . 6}.
	
	self assert: result equals: 12.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 . 6}.
	
]
