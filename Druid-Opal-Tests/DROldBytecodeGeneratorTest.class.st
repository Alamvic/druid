Class {
	#name : 'DROldBytecodeGeneratorTest',
	#superclass : 'DRBytecodeGeneratorTest',
	#category : 'Druid-Opal-Tests',
	#package : 'Druid-Opal-Tests'
}

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testBasicControlFlow [

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
DROldBytecodeGeneratorTest >> testComplexMethodInline [

	self assertCompilationFor: #complexInlineMethod fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testCreateTempVectorInsideLoop [

	self assertCompilationFor: #exampleToDoInsideTemp fromClass: OCOpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodAssignmentExpression [

	self assertCompilationFor: #methodWithAssignmentExpression fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodEmptyIf [

	self assertCompilationFor: #emptyIf fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInline [

	self assertCompilationFor: #basicInlineMethod fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineBlockTwoOuterScopes [

	self assertCompilationFor: #methodBlockTwoOuterScopes fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineFailsWithUnresolvedNonLocalReturns [

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
DROldBytecodeGeneratorTest >> testMethodInlineLoop [

	self assertCompilationFor: #methodWithoutInnerDependencies fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineObjectAccessInstVar [

	self assertCompilationFor: #basicInlineMethodAccesInstVar fromClass: DROpalExamples checkByteCode: 'send: #instVarAt:'
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineObjectSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSettingInstVar fromClass: DROpalExamples checkByteCode: 'send: #instVarAt:put:'
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineSelfAccessInstVar [

	self assertCompilationFor: #basicInlineMethodSelfAccesInstVar fromClass: DROpalExamples checkByteCode: 'pushInstVar:'
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineSelfSettingInstVar [

	self assertCompilationFor: #basicInlineMethodSelfSettingInstVar fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarOnReturn fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
	self assertCompilationFor: #basicInlineMethodSelfSettingInstVarWithUsers fromClass: DROpalExamples checkByteCode: 'storeInstVar:'.
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineToDo [

	self assertCompilationFor: #methodWithToDo fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlineToDoInner [

	self assertCompilationFor: #methodWithTwoToDos fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlinedWhileTrue [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrue fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodInlinedWhileTrueWithTemp [

	self assertCompilationFor: #simpleMethodWithBlockWhileTrueWithTemp fromClass: DROpalExamples 
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodManyReturns [

	self assertCompilationFor: #methodWithThreeNonLocalReturn fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodReadingClassVariable [

	self assertCompilationFor: #methodReadingClassVariable fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodSimpleBlockLocalWhile [

	self assertCompilationFor: #exampleWhileWithTemp fromClass: OCOpalExamples 
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithArg [

	| method result |
	method := self generateMethodForSelector: #doubleOf:.
	result := method valueWithReceiver: nil arguments: { 12 }.

	self assert: result equals: 24
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockActivated [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivated
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockActivatedArg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockActivatedArg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWithArg fromClass: DROpalExamples arguments: {}.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockArgWithBlockArg [
	
	self assertCompilationNoInlineFor: #simpleMethodArgWithBlockWithArg: fromClass: DROpalExamples arguments: { nil }.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockConstant [
	
	self assertCompilationNoInlineFor: #methodBlockConstant fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockCulled [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockCulled1Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull1Arg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockCulled2Arg [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlockCull2Arg
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockEmpty [
	
	self assertCompilationNoInlineFor: #methodEmptyBlock fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockLocalAndNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockLocalAndNonLocalReturn fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWrite [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWrite fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadAndWriteWithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadAndWriteWithArg: fromClass: DROpalExamples arguments: { 5 }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg: fromClass: DROpalExamples arguments: {1}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterArg2 [

	self
		assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterArg2:
		fromClass: DROpalExamples
		arguments: { 1 }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2ReadingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2ReadingOuterTemp2 fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2Simple [
	
	self assertCompilationNoInlineFor: #methodBlockNested2Simple fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2WithArg2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WithArg2 fromClass: DROpalExamples arguments: { }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNested2WritingOuterTemp2 [
	
	self assertCompilationNoInlineFor: #methodBlockNested2WritingOuterTemp2 fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockNonLocalReturn [
	
	self assertCompilationNoInlineFor: #methodBlockNonLocalReturn fromClass: DROpalExamples arguments: { }.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockReadingArgVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingArgVariable: fromClass: DROpalExamples arguments: { 0 }.

	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockReadingVar [

	self assertCompilationNoInlineFor: #simpleMethodWithBlockReadingOuterVariable fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockTemp [

	self
		assertCompilationNoInlineFor: #simpleMethodWithBlock
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithBlockWritingVar [
	
	self assertCompilationNoInlineFor: #simpleMethodWithBlockWritingOuterVariable fromClass: DROpalExamples arguments: {}.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithDynamicArray [

	self assertCompilationFor: #methodWithDynamicArray fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithEarlyReturn [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 0 }.

	self assert: result equals: 42
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithEarlyReturn2 [

	| method result |
	method := self generateMethodForSelector: #methodWithEarlyReturn:.
	result := method valueWithReceiver: nil arguments: { 1 }.

	self assert: result equals: 57
]

{ #category : 'tests' }
DROldBytecodeGeneratorTest >> testMethodWithPragmaPrimitive [

	self
		assertCompilationNoInlineFor: #simpleMethodWithPragmaPrimitive
		fromClass: DROpalExamples
		arguments: {  }
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testMethodWithTypeCheck [

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
DROldBytecodeGeneratorTest >> testMethodWritingClassVariable [

	self assertCompilationFor: #methodWritingClassVariable fromClass: DROpalExamples
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testOpalExamples [

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
DROldBytecodeGeneratorTest >> testSimpleMethod [
	
	| compiledMethod expectedMethod aSelector |
	aSelector:= #simpleMethod.
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	self assert: compiledMethod equalsResult: expectedMethod
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testSimpleMethodWith1arg [
	
	| compiledMethod expectedMethod aSelector result|
	aSelector:= #simpleMethodWith1Arg:.
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	result := compiledMethod valueWithReceiver: DROpalExamples new arguments: { 5 }.
	
	self assert: result equals: 6.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 }.
	
]

{ #category : 'vrac' }
DROldBytecodeGeneratorTest >> testSimpleMethodWith2arg [
	
	| compiledMethod expectedMethod aSelector result |
	aSelector:= #simpleMethodWith1Arg:arg2:.
	expectedMethod := DROpalExamples >> aSelector.
	
	compiledMethod := self compileSelector: aSelector fromClass: DROpalExamples.
	
	result := compiledMethod valueWithReceiver: DROpalExamples new arguments: { 5 . 6}.
	
	self assert: result equals: 12.
	self assert: compiledMethod equalsResult: expectedMethod arguments: { 5 . 6}.
	
]
