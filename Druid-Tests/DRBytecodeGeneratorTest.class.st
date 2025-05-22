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
	cfg := DRControlFlowGraph new.
	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].

	method := self generateMethodFromCFG: cfg withSelector: #m numArgs: 0.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 7
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationAllSatisfy [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #allSatisfy:.
	method := Array lookupSelector: selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2 } arguments: { [ :e | e even ] }.
	self assert: result equals: false.

	result := method valueWithReceiver: { 2. 4 } arguments: { [ :e | e even ] }.
	self assert: result equals: true.

"
	{ Array } do: [ :e |
		e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
		e addSelectorSilently: selector withMethod: method ]
	"
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationAnySatisfy [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #anySatisfy:.
	method := Array lookupSelector: selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.
	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2 } arguments: { [ :e | e even ] }.
	self assert: result equals: true.
	
	result := method valueWithReceiver: { 1 . 3 } arguments: { [ :e | e even ] }.
	self assert: result equals: false.

"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationBadCount [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler irGenerator typeSystem: typeSystem.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #badCount.
	method := Array lookupSelector: selector.

	compilerCompiler interpreter: Array new.

	cfg := self generateDruidIRFor: method.

	compilerCompiler optimize: cfg.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2 } arguments: {}.
	self assert: result equals: 2.

	result := method valueWithReceiver: { 2. 4. 6 } arguments: {}.
	self assert: result equals: 3.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationDifference [

	| method result cfg selector |
	
	self skip.
	
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #difference:.
	method := Set >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method valueWithReceiver: { 1. 2. 3 } asSet arguments: { { 3 . 4 . 5 } asSet }.

	self assertCollection: result includesAll: { 1. 2 }.


"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationDo [

	| method result cfg selector |
	typeSystem := DRCustomisationTypeSystem new.
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #do:.
	method := SequenceableCollection >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := OrderedCollection new.
	method valueWithReceiver: { 1. 2. 3 } arguments: { [ :e | result add: e ] }.

	self assertCollection: result includesAll: { 1. 2. 3 }.


"	{ Array }	
		do: [ :e |
			e addAndClassifySelector: selector withMethod: method inProtocol: '*DrOpal-Extensions'.
			e addSelectorSilently: selector withMethod: method ]"
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodCustomisationSelect [

	| method result cfg selector |
	compilerCompiler irGenerator typeSystem:
		(typeSystem := DRCustomisationTypeSystem new).
	compilerCompiler configureForCompilerClass: nil.
	bytecodeGenerator := DRBytecodeGenerator new.
	selector := #select:.
	method := Collection >> selector.

	compilerCompiler interpreter: {  }.
	cfg := self generateDruidIRFor: method.
	compilerCompiler optimize: cfg.

	method := self generateMethodFromCFG: cfg withSelector: selector.

	result := method
		          valueWithReceiver: { 1. 2. 3 } asOrderedCollection
		          arguments: { [ :e | e even ] }.

	self assertCollection: result includesAll: { 2 }.
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

	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DruidTestInterpreter checkByteCode: 'send: #instVarAt:'
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

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
DRBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #methodBlockConstant .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #methodEmptyBlock .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #methodBlockLocalAndNonLocalReturn .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #methodBlockNonLocalReturn .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
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
DRBytecodeGeneratorTest >> testOpalBooleanMethods [

	OCOpalExamples methods
		select: [ :opalMethod | opalMethod protocolName = #'examples - andor' ]
		thenDo: [ :opalMethod | self assertCompilationFor: opalMethod selector fromClass: OCOpalExamples ]
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

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWithBlock [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #simpleMethodWithBlock.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWithBlockArg [
	
	| compiledMethod expectedMethod aSelector |

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #simpleMethodWithBlockWithArg.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWithBlockReadingVar [
	
	| compiledMethod expectedMethod aSelector |

	self skip. "in this case, no vectorTemp is supposed to be created on the outerScope"

	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #simpleMethodWithBlockReadingOuterVariable.
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> testSimpleMethodWithBlockWritingVar [
	
	| compiledMethod expectedMethod aSelector |
	
	compilerCompiler configureForCompilerClassWithoutInline: nil.
	aSelector:= #simpleMethodWithBlockWritingOuterVariable .
	expectedMethod := DruidTestInterpreter >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DruidTestInterpreter.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]
