Class {
	#name : 'DRBytecodeGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator',
		'typeSystem'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
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
	self assert: result equals: expectedResult
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
	
	self assert: compiledMethod equalsResult: expectedMethod arguments: args
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

	^ self generateDruidIRFor: (DruidTestInterpreter lookupSelector: selector)
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
	aDRControlFlowGraph applyOptimisation: (DRBranchCollapse then: DRPhiSimplication).
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
	compilerCompiler interpreter: DruidTestInterpreter basicNew.
	
	bytecodeGenerator := DRBytecodeGenerator new.
	typeSystem := DRPragmaBasedTypeSystem new
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

	self assertCompilationFor: #basicInlineMethod fromClass: DruidTestInterpreter
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineObjectAccessInstVar [

	self skip. "need to repass this after we handle inlining"
	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DruidTestInterpreter checkByteCode: 'send: #instVarAt:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

	self skip. "need to repass this after we handle inlining"
	self assertCompilationFor: #basicInlineMethodSettingInstVar fromClass: DruidTestInterpreter checkByteCode: 'send: #instVarAt:put:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineSelfAccessInstVar [

	self assertCompilationFor: #basicInlineMethodSelfAccesInstVar fromClass: DruidTestInterpreter checkByteCode: 'pushInstVar:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineSelfSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSelfSettingInstVar fromClass: DruidTestInterpreter checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarOnReturn fromClass: DruidTestInterpreter checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarWithUsers fromClass: DruidTestInterpreter checkByteCode: 'storeInstVar:'.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DruidTestInterpreter
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodRecompilationWithProfile [

	| method result cfg selector methodProfiler previousMethodSize |
	
	self skip. "This is a proof of concept, does not metter"
	
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	typeSystem := DRProfileBasedTypeSystem reset.
	compilerCompiler configureForCompilerClass: nil.
	selector := #methodCollectionSize:.
	method := self generateMethodForSelector: selector.

	previousMethodSize := (DruidTestInterpreter >> selector) size.

	self assert: method size > previousMethodSize. "Bigger than original"
	previousMethodSize := method size.

	"First execution"
	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	"Profiled type"
	self assert: typeSystem profileTable size equals: 1.
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast arguments first)
		hasSameElements: { (DRClassType for: OrderedCollection) }.

	"Recompile"
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size > previousMethodSize. "Bigger because of new guard"
	previousMethodSize := method size.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4.

	"New profiled type"
	self assert: typeSystem profileTable size equals: 2. "Old and new methods"
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast temporaries first)
		hasSameElements: { (DRClassType for: Array2D) }.


	"Recompile again"
	compilerCompiler := DRMethodCompilerCompiler new.
	bytecodeGenerator := DRProfileBytecodeGenerator new.
	cfg := self generateDruidIRFor: method.
	method := self generateMethodFromCFG: cfg withSelector: selector.

	self assert: method size equals: 144. "bigger then before"

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	result := method
		          valueWithReceiver: nil
		          arguments: { (Array2D rows: 2 columns: 2) }.
	self assert: result equals: 4.

	"Last profiled type"
	self assert: typeSystem profileTable size equals: 3. "New type from Array2D inline"
	methodProfiler := typeSystem profileTable at: method ast.
	self
		assertCollection:
		(methodProfiler typesFor: method ast temporaries second)
		hasSameElements: { (DRClassType for: Array) }
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
		fromClass: DruidTestInterpreter
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivatedArg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivatedArg
		fromClass: DruidTestInterpreter
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWithArg fromClass: DruidTestInterpreter arguments: {}.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockArgWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodArgWithBlockWithArg: fromClass: DruidTestInterpreter arguments: { nil }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	self assertCompilationNoInlineFor: #methodBlockConstant fromClass: DruidTestInterpreter arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	self assertCompilationNoInlineFor: #methodEmptyBlock fromClass: DruidTestInterpreter arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockLocalAndNonLocalReturn fromClass: DruidTestInterpreter arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWrite [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWrite fromClass: DruidTestInterpreter arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWriteWithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWriteWithArg: fromClass: DruidTestInterpreter arguments: { 5 }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg: fromClass: DruidTestInterpreter arguments: {1}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg2: fromClass: DruidTestInterpreter arguments: {1}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp2 fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2Simple [
	
	self assertCompilationNoInlineFor: #methodBlockNested2Simple fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg fromClass: DruidTestInterpreter arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg2 fromClass: DruidTestInterpreter arguments: { }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp2 fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockNonLocalReturn fromClass: DruidTestInterpreter arguments: { }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingArgVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingArgVariable: fromClass: DruidTestInterpreter arguments: { nil }.

	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingVar [

	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingOuterVariable fromClass: DruidTestInterpreter arguments: {}.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockTemp [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlock
		fromClass: DruidTestInterpreter
		arguments: {  }
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockWritingVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWritingOuterVariable fromClass: DruidTestInterpreter arguments: {}.
	
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
	
	self skip: 'Depends on the inlining'.
	
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
DRBytecodeGeneratorTest >> testOpalSimpleMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - simple' ]
		thenDo: [ :opalMethod | 
			self setUp.
			self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethod [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethod.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodReturnArg [
	
	| compiledMethod expectedMethod aSelector result |

	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodReturnArg: .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 5.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWith1arg [
	
	| compiledMethod expectedMethod aSelector result|
	
	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodWith1Arg:.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 6.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWith2arg [
	
	| compiledMethod expectedMethod aSelector result |
	
	compilerCompiler configureForCompilerClass: nil.
	aSelector:= #simpleMethodWith1Arg:arg2:.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 . 6}.
	
	self assert: result equals: 12.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 . 6}.
	
]
