Class {
	#name : 'DRBytecodeGeneratorTest',
	#superclass : 'TestCase',
	#instVars : [
		'compilerCompiler',
		'bytecodeGenerator',
		'typeSystem',
		'counter'
	],
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method [

	self assert: compiledMethod equalsResult: method arguments: {  }
]

{ #category : 'asserting' }
DRBytecodeGeneratorTest >> assert: compiledMethod equalsResult: method arguments: arguments [

	| result expectedResult receiver |
	receiver := compiledMethod methodClass new.

	receiver initialize.
	result := compiledMethod valueWithReceiver: receiver arguments: arguments.

	receiver initialize.
	expectedResult := method valueWithReceiver: receiver arguments: arguments.

	expectedResult isClosure ifTrue: [
		^ self assertClosure: result equals: expectedResult ].

	(expectedResult isCollection and: [
		 expectedResult isNotEmpty and: [ expectedResult anyOne isClosure ] ])
		ifTrue: [
			self assert: result size equals: expectedResult size.
			expectedResult doWithIndex: [ :elem :i |
				self assertClosure: (result at: i) equals: elem ].
			^ self ].

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
	expectedMethod := aClass >> aSelector.
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: compiledMethod equalsResult: expectedMethod
]

{ #category : 'assertion' }
DRBytecodeGeneratorTest >> assertCompilationFor: aSelector fromClass: aClass checkByteCode: byteCode [

	| compiledMethod expectedMethod |
	self setUp.
		
	expectedMethod := aClass >> aSelector.
	compiledMethod := self compileSelector: aSelector fromClass: aClass.
	
	self assert: (self checkByteCode: byteCode in: compiledMethod ir).
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

	^ DROpalCompiler 
		  generateMethodFromCFG: aDRControlFlowGraph
		  withSelector: selector
		  numArgs: compilerCompiler irGenerator numberOfArguments
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> incrementCounter [

	counter := counter + 1
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> setUp [
	super setUp.
	
	compilerCompiler := DRMethodCompilerCompiler new.
	compilerCompiler interpreter: DrOpalExamples basicNew.
	compilerCompiler configureForCompilerClass: nil.
	
	bytecodeGenerator := DRBytecodeGenerator new.
	
	counter := 0
]

{ #category : 'running' }
DRBytecodeGeneratorTest >> tearDown [

	| aPackage |
	aPackage := PackageOrganizer default packageNamed: 'DrOpal-Extensions' ifAbsent: nil.
	aPackage ifNotNil: [ aPackage removeFromSystem ].
	
	super tearDown.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test001_emptyMethodReturnsSelf [

	| newMethod receiver result |
	newMethod := self compileSelector: #empty fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result identicalTo: receiver
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test002_returnLiteralInteger [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnLiteralInteger fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 1
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test003_returnArgument [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnArgument: fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver arguments: {17}.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test004_returnUninitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnUninitializedTemp fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: nil
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test005_returnInitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInitializedTemp fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test006_returnTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnTempAssignmentExpression fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test007_returnNestedTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedTempAssignmentExpression fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test008_returnInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariable fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	receiver dummyVariable: 178.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 178
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test009_setInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #setInstanceVariable fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: receiver dummyVariable equals: 179
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test010_returnInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariableAssignmentExpression fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test011_returnNestedInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedInstanceVariableAssignmentExpression fromClass: DrOpalExamples.
	
	receiver := DrOpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17.
	self assert: receiver fillMask equals: 17.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test012_sendMessage [

	| newMethod |
	newMethod := self compileSelector: #sendMessage fromClass: DrOpalExamples.
	
	newMethod valueWithReceiver: self.
	
	self assert: counter equals: 1
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testBasicControlFlow [

	| cfg method result |
	cfg := DRMethodControlFlowGraph new.
	cfg newBasicBlockWith: [ :b | 
		cfg initialBasicBlock jumpTo: b.
		b return: (b add: 3 to: 4) ].
	cfg scope: DRScope new.
	method := DROpalCompiler generateMethodFromCFG: cfg withSelector: #m numArgs: 0.
	result := method valueWithReceiver: nil arguments: {  }.

	self assert: result equals: 7
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testComplexMethodInline [

	self assertCompilationFor: #complexInlineMethod fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testCreateTempVectorInsideLoop [

	self assertCompilationFor: #exampleToDoInsideTemp fromClass: OCOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodAssignmentExpression [

	self assertCompilationFor: #methodWithAssignmentExpression fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodEmptyIf [

	self assertCompilationFor: #emptyIf fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInline [

	self assertCompilationFor: #basicInlineMethod fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineBlockTwoOuterScopes [

	self assertCompilationFor: #methodBlockTwoOuterScopes fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineFailsWithUnresolvedNonLocalReturns [

	| inline |
	inline := compilerCompiler optimisations removeFirst.
	self assert: inline class equals: DRInline.

	self
		should: [
				self
					assertCompilationFor: #methodInlineUnresolvedBlockNonLocalReturn
					fromClass: DrOpalExamples ]
		raise: Error
		withExceptionDo: [ :err |
			self assert: (err messageText beginsWith: 'Non-local return') ]
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineLoop [

	self assertCompilationFor: #methodWithoutInnerDependencies fromClass: DrOpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineObjectAccessInstVar [

	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DrOpalExamples checkByteCode: 'send: #instVarAt:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSettingInstVar fromClass: DrOpalExamples checkByteCode: 'send: #instVarAt:put:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineSelfAccessInstVar [

	self assertCompilationFor: #basicInlineMethodSelfAccesInstVar fromClass: DrOpalExamples checkByteCode: 'pushInstVar:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineSelfSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSelfSettingInstVar fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarOnReturn fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarWithUsers fromClass: DrOpalExamples checkByteCode: 'storeInstVar:'.
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineToDo [

	self assertCompilationFor: #methodWithToDo fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineToDoInner [

	self assertCompilationFor: #methodWithTwoToDos fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlinedWhileTrue [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrue fromClass: DrOpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlinedWhileTrueWithTemp [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrueWithTemp fromClass: DrOpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodReadingClassVariable [

	self assertCompilationFor: #methodReadingClassVariable fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodSimpleBlockLocalWhile [

	self assertCompilationFor: #exampleWhileWithTemp fromClass: OCOpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithArg [

	| method result |
	method := self generateMethodForSelector: #doubleOf:.
	result := method valueWithReceiver: nil arguments: { 12 }.

	self assert: result equals: 24
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivated [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivated
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivatedArg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivatedArg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWithArg fromClass: DrOpalExamples arguments: {}.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockArgWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodArgWithBlockWithArg: fromClass: DrOpalExamples arguments: { nil }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	self assertCompilationNoInlineFor: #methodBlockConstant fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled1Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull1Arg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled2Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull2Arg
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	self assertCompilationNoInlineFor: #methodEmptyBlock fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockLocalAndNonLocalReturn fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWrite [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWrite fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWriteWithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWriteWithArg: fromClass: DrOpalExamples arguments: { 5 }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg: fromClass: DrOpalExamples arguments: {1}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg2 [

	self
		assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg2:
		fromClass: DrOpalExamples
		arguments: { 1 }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp2 fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2Simple [
	
	self assertCompilationNoInlineFor: #methodBlockNested2Simple fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg2 fromClass: DrOpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp2 fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockNonLocalReturn fromClass: DrOpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingArgVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingArgVariable: fromClass: DrOpalExamples arguments: { 0 }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingVar [

	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingOuterVariable fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockTemp [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlock
		fromClass: DrOpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockWritingVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWritingOuterVariable fromClass: DrOpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithDynamicArray [

	self assertCompilationFor: #methodWithDynamicArray fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 0 }.

	self assert: result equals: 42
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithEarlyReturn2 [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 1 }.

	self assert: result equals: 57
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithTypeCheck [

	| method result |
	method := self generateMethodForSelector: #methodCollectionSize:.

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asOrderedCollection }.
	self assert: result equals: 3.

	result := method
		          valueWithReceiver: nil
		          arguments: { { 1. 2. 3 } asBag  }.
	self assert: result equals: 3
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWritingClassVariable [

	self assertCompilationFor: #methodWritingClassVariable fromClass: DrOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testOpalExamples [

	| ignoredProtocols |
	ignoredProtocols := #( #'examples - pragmas' ).

	OCOpalExamples methods
		reject: [ :opalMethod |
				opalMethod numArgs > 0 or: [
					ignoredProtocols includes: opalMethod protocolName ] ]
		thenDo: [ :opalMethod |
				self setUp.
				self
					assertCompilationFor: opalMethod selector
					fromClass: OCOpalExamples ]
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testSimpleMethod [
	
	| compiledMethod expectedMethod aSelector |
	aSelector:= #simpleMethod.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testSimpleMethodWith1arg [
	
	| compiledMethod expectedMethod aSelector result|
	aSelector:= #simpleMethodWith1Arg:.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 6.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testSimpleMethodWith2arg [
	
	| compiledMethod expectedMethod aSelector result |
	aSelector:= #simpleMethodWith1Arg:arg2:.
	expectedMethod := DrOpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DrOpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 . 6}.
	
	self assert: result equals: 12.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 . 6}.
	
]
