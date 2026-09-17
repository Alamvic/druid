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

	^ self generateDruidIRFor: (DROpalExamples lookupSelector: selector)
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
	compilerCompiler interpreter: DROpalExamples basicNew.
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
	newMethod := self compileSelector: #empty fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result identicalTo: receiver
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test002_returnLiteralInteger [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnLiteralInteger fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 1
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test003_returnArgument [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnArgument: fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver arguments: {17}.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test004_returnUninitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnUninitializedTemp fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: nil
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test005_returnInitializedTemp [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInitializedTemp fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test006_returnTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnTempAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test007_returnNestedTempAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedTempAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test008_returnInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariable fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	receiver dummyVariable: 178.
	result := newMethod valueWithReceiver: receiver.
	self assert: result equals: 178
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test009_setInstanceVariable [

	| newMethod receiver result |
	newMethod := self compileSelector: #setInstanceVariable fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	self assert: receiver dummyVariable equals: 179
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test010_returnInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnInstanceVariableAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test011_returnNestedInstanceVariableAssignmentExpression [

	| newMethod receiver result |
	newMethod := self compileSelector: #returnNestedInstanceVariableAssignmentExpression fromClass: DROpalExamples.
	
	receiver := DROpalExamples new.
	result := newMethod valueWithReceiver: receiver.
	
	self assert: result equals: 17.
	self assert: receiver dummyVariable equals: 17.
	self assert: receiver fillMask equals: 17.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test012_sendMessage [

	| newMethod |
	newMethod := self compileSelector: #sendMessage fromClass: DROpalExamples.
	
	newMethod valueWithReceiver: self.
	
	self assert: counter equals: 1
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test013_returnIfTrueIfFalseValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueIfFalseValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test014_returnIfTrueValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: nil
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test015_returnIfTrueNonLocalReturn [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #returnIfTrueNonLocalReturn: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test016_storeIfTrueIfFalseValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrueIfFalseValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test017_storeIfTrueValue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrueValue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: nil
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test018_storeInstVarIfTrueIfFalseValue [

	| newMethod instance |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeInstVarIfTrueIfFalseValue: fromClass: DROpalExamples.

	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { true }.
	self assert: instance dummyVariable equals: 5.
	
	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { false }.
	self assert: instance dummyVariable equals: 42.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test019_storeInstVarIfTrueValue [

	| newMethod instance |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeInstVarIfTrueValue: fromClass: DROpalExamples.

	instance := DROpalExamples new.
	newMethod valueWithReceiver: instance arguments: { true }.
	self assert: instance dummyVariable equals: 5.

	newMethod valueWithReceiver: instance arguments: { false }.
	self assert: instance dummyVariable equals: nil.
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test020_storeIfTrue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeIfTrue: fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self arguments: { true }) equals: 5.
	self assert: (newMethod valueWithReceiver: self arguments: { false }) equals: 42
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test021_storeWhileTrue [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeWhileTrue fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self) equals: 5
]

{ #category : 'tests' }
DRBytecodeGeneratorTest >> test022_storeToDo [

	| newMethod |
	compilerCompiler irGenerator typeSystem: DRCustomisationTypeSystem new.
	newMethod := self compileSelector: #storeToDo fromClass: DROpalExamples.

	self assert: (newMethod valueWithReceiver: self) equals: 5
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

	self assertCompilationFor: #complexInlineMethod fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testCreateTempVectorInsideLoop [

	self assertCompilationFor: #exampleToDoInsideTemp fromClass: OCOpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodAssignmentExpression [

	self assertCompilationFor: #methodWithAssignmentExpression fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodEmptyIf [

	self assertCompilationFor: #emptyIf fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInline [

	self assertCompilationFor: #basicInlineMethod fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineBlockTwoOuterScopes [

	self assertCompilationFor: #methodBlockTwoOuterScopes fromClass: DROpalExamples
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
					fromClass: DROpalExamples ]
		raise: Error
		withExceptionDo: [ :err |
			self assert: (err messageText beginsWith: 'Non-local return') ]
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineLoop [

	self assertCompilationFor: #methodWithoutInnerDependencies fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineObjectAccessInstVar [

	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DROpalExamples checkByteCode: 'send: #instVarAt:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSettingInstVar fromClass: DROpalExamples checkByteCode: 'send: #instVarAt:put:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineSelfAccessInstVar [

	self assertCompilationFor: #basicInlineMethodSelfAccesInstVar fromClass: DROpalExamples checkByteCode: 'pushInstVar:'
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineSelfSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSelfSettingInstVar fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarOnReturn fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarWithUsers fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineToDo [

	self assertCompilationFor: #methodWithToDo fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlineToDoInner [

	self assertCompilationFor: #methodWithTwoToDos fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlinedWhileTrue [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrue fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodInlinedWhileTrueWithTemp [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrueWithTemp fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DROpalExamples
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodReadingClassVariable [

	self assertCompilationFor: #methodReadingClassVariable fromClass: DROpalExamples
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
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockActivatedArg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivatedArg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWithArg fromClass: DROpalExamples arguments: {}.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockArgWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodArgWithBlockWithArg: fromClass: DROpalExamples arguments: { nil }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	self assertCompilationNoInlineFor: #methodBlockConstant fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled1Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull1Arg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockCulled2Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull2Arg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	self assertCompilationNoInlineFor: #methodEmptyBlock fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockLocalAndNonLocalReturn fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWrite [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWrite fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWriteWithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWriteWithArg: fromClass: DROpalExamples arguments: { 5 }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg: fromClass: DROpalExamples arguments: {1}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg2 [

	self
		assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg2:
		fromClass: DROpalExamples
		arguments: { 1 }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp2 fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2Simple [
	
	self assertCompilationNoInlineFor: #methodBlockNested2Simple fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg2 fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp2 fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockNonLocalReturn fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingArgVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingArgVariable: fromClass: DROpalExamples arguments: { 0 }.

	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockReadingVar [

	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingOuterVariable fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockTemp [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlock
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithBlockWritingVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWritingOuterVariable fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testMethodWithDynamicArray [

	self assertCompilationFor: #methodWithDynamicArray fromClass: DROpalExamples
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

	self assertCompilationFor: #methodWritingClassVariable fromClass: DROpalExamples
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
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testSimpleMethodWith1arg [
	
	| compiledMethod expectedMethod aSelector result|
	aSelector:= #simpleMethodWith1Arg:.
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 }.
	
	self assert: result equals: 6.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
]

{ #category : 'vrac' }
DRBytecodeGeneratorTest >> testSimpleMethodWith2arg [
	
	| compiledMethod expectedMethod aSelector result |
	aSelector:= #simpleMethodWith1Arg:arg2:.
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	result := compiledMethod valueWithReceiver: compilerCompiler interpreter arguments: { 5 . 6}.
	
	self assert: result equals: 12.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 . 6}.
	
]
