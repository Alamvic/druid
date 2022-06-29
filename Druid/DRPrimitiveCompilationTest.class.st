Class {
	#name : #DRPrimitiveCompilationTest,
	#superclass : #DRDruidTests,
	#instVars : [
		'stopAddress'
	],
	#category : #'Druid-Tests'
}

{ #category : #'building suites' }
DRPrimitiveCompilationTest class >> testParameters [

	"IA32 disabled because the current compiler puts a lot of pressure on register allocation"
	^ ParametrizedTestMatrix new
		addCase: { #ISA -> #'aarch64'. #wordSize -> 8};
		addCase: { #ISA -> #'X64'. #wordSize -> 8};
		addCase: { #ISA -> #'ARMv5'. #wordSize -> 4};
		yourself
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector [
	
	| generatorSelector stopInstruction |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.
	"First generate druid code"
	DRPrimitiveCompilerCompiler
		compilePrimitive: aSelector
		from: self interpreter
		as: generatorSelector
		in: DruidTestRTLCompiler.

	"Then generate the machine code for that method"
	initialAddress := self compile: [
		cogit methodOrBlockNumArgs: 2.
		cogit perform: generatorSelector.
		stopInstruction := cogit Stop
	].
	stopAddress := stopInstruction address
]

{ #category : #helpers }
DRPrimitiveCompilationTest >> executeUntilStopPrimitiveWithReceiver: receiverOop [
	
	^ self executeUntilStopPrimitiveWithReceiver: receiverOop withArguments: { }
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> executeUntilStopPrimitiveWithReceiver: receiverOop withArguments: aCollection [
	
	"Simulate a primitive execution having an object as receiver and a single argument
	  - the receiver goes to the receiver register
	  - the argument should be pushed to the stack
	If we are in a system without a link register, we need to push the caller IP to the stack to simulate a call"
	
	self prepareStackForSendReceiver: receiverOop arguments: aCollection.
	self runFrom: initialAddress until: stopAddress.
]

{ #category : #'tests-methodactivations' }
DRPrimitiveCompilationTest >> testAccessTemporaryAfterCall [

	| receiver |
	self compileDruidPrimitive: #primitiveAccessTempAfterCall.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testAdditionIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveSumWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 52)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testAdditionIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveSumWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory maxSmallInteger) withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskFalse [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskFalseInverted [

	self compileDruidPrimitive: #primitiveConditionWithAnyMaskInverted.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskTrue [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 1

]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskTrueInverted [

	self compileDruidPrimitive: #primitiveConditionWithAnyMaskInverted.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 1

]

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testAssertIsIgnored [

	self compileDruidPrimitive: #primitiveAssertIsIgnored.

	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 17

]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testBranchingWithAssigmentsFalse [

	self compileDruidPrimitive: #primitiveBranchingWithAssigments.
	
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testBranchingWithAssigmentsTrue [

	self compileDruidPrimitive: #primitiveBranchingWithAssigments.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompileEmptyPrimitiveWithArgumentsReturnsReceiver [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitive.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: receiver.
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompileEmptyPrimitiveWithoutArgumentsReturnsTopOfTheStack [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitiveWithArguments.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver withArgument: memory trueObject.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: receiver
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompileNonCompiledCaseExitsWithFailure [

	| receiver |
	self compileDruidPrimitive: #primitiveWithNonCompilableBranch.
	
	receiver := memory integerObjectOf: -1.
	
	self executeUntilStopPrimitiveWithReceiver: receiver.
	
	"Should stop at the stop and not timeout"
	self assert: machineSimulator instructionPointerRegisterValue equals: stopAddress
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompileNonCompiledCaseExitsWithSuccess [

	| receiver |
	self compileDruidPrimitive: #primitiveWithNonCompilableBranch.
	
	receiver := memory integerObjectOf: 1.
	
	self executePrimitiveWithReceiver: receiver.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-array' }
DRPrimitiveCompilationTest >> testCompilePrimitiveAt [

	| class array |
	
	true ifTrue: [ ^self skip ].
	
	self timeLimit: (Duration minutes: 4).
	self compileDruidPrimitive: #primitiveAt.

	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.
	memory storePointer: 0 ofObject: array withValue: 42.

	self 	executePrimitiveWithReceiver: array 
			withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
	
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingBlock [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReturningConstant.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingBlockReadingExternalTemp [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReadingExternalTemp.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingBlockWithArgument [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReturningArgument.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingBlockWithNonLocalReturn [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockWithNonLocalReturn.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingBlockWritingExternalTemp [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockWritingExternalTemp.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingEmptyBlock [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingEmptyBlock.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 18
]

{ #category : #'tests-methodactivations' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingMethod [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingMethodReturningConstant.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 73
]

{ #category : #'tests-methodactivations' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingMethodWithEarlyReturnCaseFalse [

	self compileDruidPrimitive: #primitiveCallingMethodWithEarlyReturn.
	
	self executePrimitiveWithReceiver: 1237123.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-methodactivations' }
DRPrimitiveCompilationTest >> testCompilePrimitiveCallingMethodWithEarlyReturnCaseTrue [

	self compileDruidPrimitive: #primitiveCallingMethodWithEarlyReturn.
	
	self executePrimitiveWithReceiver: 0.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-array' }
DRPrimitiveCompilationTest >> testCompilePrimitiveNew [

	| class array |
	true ifTrue: [ ^self skip ].

	self timeLimit: (Duration minutes: 2).
	self compileDruidPrimitive: #primitiveNew.
	
	class := self newClassInOldSpaceWithSlots: 0 instSpec: memory arrayFormat.
	array := memory instantiateClass: class indexableSize: 1.
	memory storePointer: 0 ofObject: array withValue: 42.

	self 	executePrimitiveWithReceiver: array 
			withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
	
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompilePrimitiveReturningConstant [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnOne.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompilePrimitiveReturningFalseObjectReference [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnFalse.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	self assert: machineSimulator receiverRegisterValue equals: memory falseObject
]

{ #category : #'tests-basic' }
DRPrimitiveCompilationTest >> testCompilePrimitiveReturningObjectReference [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnTrue.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #'tests-stack' }
DRPrimitiveCompilationTest >> testCompiledPrimitiveInitializesCorrectlyTheSimulatedStack [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnOneWithArguments.
	
	receiver := memory integerObjectOf: 43.
	self pushAddress: 55.
	self executePrimitiveWithReceiver: receiver withArguments: { 1. 2 }.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-stack' }
DRPrimitiveCompilationTest >> testCompiledPrimitivePreservesStack [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitive.
	
	receiver := memory integerObjectOf: 43.
	self pushAddress: 55.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: self popAddress equals: 55
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testDecrement [

	self compileDruidPrimitive: #primitiveDecrement.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 41
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testDivision [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testDivisionByZero [

	<expectedFailure>

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 0 }.
	
	self flag: #todo. "Fix simulator"
	self assert: machineSimulator overflow "Replace with expected assertion"
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testDivisionNotExact [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testDivisionNotExactNegative [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { -3 }.
	
	self assert: (machineSimulator receiverRegisterValue signedIntFromShort) equals: -1.   
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testEqualsThan [

	self compileDruidPrimitive: #primitiveEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testEqualsThanFail [

	self compileDruidPrimitive: #primitiveEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testEqualsThanInverted [

	self compileDruidPrimitive: #primitiveEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testEqualsThanInvertedFail [

	self compileDruidPrimitive: #primitiveEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testEqualsThanValueFalse [

	self compileDruidPrimitive: #primitiveEqualsThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testEqualsThanValueTrue [

	self compileDruidPrimitive: #primitiveEqualsThanValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testFailingPrimitive2ExecutesUntilStop [

	self compileDruidPrimitive: #failingPrimitive2.
	
	self executeUntilStopPrimitiveWithReceiver: 1
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testFailingPrimitiveWithArgExecutesUntilStop [

	self compileDruidPrimitive: #failingPrimitiveWithArg.
	
	self executeUntilStopPrimitiveWithReceiver: 1 withArguments: { 42 }
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testFailingSuccessExecutesUntilStop [

	self compileDruidPrimitive: #failingSuccess.
	
	self executeUntilStopPrimitiveWithReceiver: 1
]

{ #category : #'tests-types' }
DRPrimitiveCompilationTest >> testFalseAsInteger [

	self compileDruidPrimitive: #primitiveFalseAsInteger.
	
	self executePrimitiveWithReceiver: 17.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testFourDeadBranchesFourBranches1 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 11.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testFourDeadBranchesFourBranches2 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 119
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testFourDeadBranchesFourBranches3 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 7.
	
	self assert: machineSimulator receiverRegisterValue equals: 146
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testFourDeadBranchesFourBranches4 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 158
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testGreaterOrEqualThanValueFalse [

	self compileDruidPrimitive: #primitiveGreaterOrEqualThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testGreaterOrEqualThanValueTrue [

	self compileDruidPrimitive: #primitiveGreaterOrEqualThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterOrEqualsThan [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterOrEqualsThanEquals [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterOrEqualsThanEqualsInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterThan [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testGreaterThanInverted [

	self compileDruidPrimitive: #primitiveGreaterThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfFalseIfTrueReturningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfFalseIfTrueReturningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfFalseIfTrueReturningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfFalseIfTrueReturningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNilNotNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNilIfNotNilStatement.
	
	"If bigger than 0, it will see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNilNotNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNilIfNotNilStatement.
	
	"If smaller than 0, it will not see nil and set 42"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilArgumentIfNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatementWithArgument.
	
	"If bigger than 0, it will see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilArgumentIfNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatementWithArgument.
	
	"If smaller than 0, it will not see nil and set 42"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilIfNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilIfNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilStatementWithNil [

	self compileDruidPrimitive: #primitiveDeadBranchWithError.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfNotNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueIfFalseAssigningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseAssigningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueIfFalseAssigningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseAssigningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueIfFalseReturningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseReturningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueIfFalseReturningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseReturningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueStatementCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueStatement.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 10
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testIfTrueStatementCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueStatement.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testIncrement [

	self compileDruidPrimitive: #primitiveIncrement.

	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #'tests-builtins' }
DRPrimitiveCompilationTest >> testIsIntegerObjectWithIntegerObject [

	self compileDruidPrimitive: #primitiveIsIntegerObject.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 0).
	
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #'tests-builtins' }
DRPrimitiveCompilationTest >> testIsIntegerObjectWithNonIntegerObject [

	self compileDruidPrimitive: #primitiveIsIntegerObject.
	self executePrimitiveWithReceiver: 0 "No tag".
	
	self assert: machineSimulator receiverRegisterValue equals: memory falseObject
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testLessOrEqualThanValueFalse [

	self compileDruidPrimitive: #primitiveLessOrEqualThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testLessOrEqualThanValueTrue [

	self compileDruidPrimitive: #primitiveLessOrEqualThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testLessThanValueFalse [

	self compileDruidPrimitive: #primitiveLessThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveCompilationTest >> testLessThanValueTrue [

	self compileDruidPrimitive: #primitiveLessThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testMod [

	self compileDruidPrimitive: #primitiveMod.

	self executePrimitiveWithReceiver: 43 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testModNegative [

	self compileDruidPrimitive: #primitiveMod.

	self executePrimitiveWithReceiver: 43 withArguments: { -2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testMultiplicationIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveMultiplyWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
		
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 420)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testMultiplicationIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveMultiplyWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory maxSmallInteger) withArguments: { memory integerObjectOf: 2 }.

	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testNegated [

	self compileDruidPrimitive: #primitiveNegated.
	
	self executePrimitiveWithReceiver: 5.

	self assert: (machineSimulator receiverRegisterValue signedIntFromShort) equals: -5
]

{ #category : #'tests-arithmetic' }
DRPrimitiveCompilationTest >> testNegatedNegative [

	self compileDruidPrimitive: #primitiveNegated.
	
	self executePrimitiveWithReceiver: -5.

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotEqualsThan [

	self compileDruidPrimitive: #primitiveNotEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotEqualsThanFail [

	self compileDruidPrimitive: #primitiveNotEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotEqualsThanInverted [

	self compileDruidPrimitive: #primitiveNotEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotEqualsThanInvertedFail [

	self compileDruidPrimitive: #primitiveNotEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotGreaterOrEqualsThan [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotGreaterOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotGreaterThan [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotGreaterThanEquals [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotGreaterThanInverted [

	self compileDruidPrimitive: #primitiveGreaterThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotSmallerOrEqualsThan [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 50.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotSmallerOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotSmallerThan [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotSmallerThanEquals [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testNotSmallerThanInverted [

	self compileDruidPrimitive: #primitiveLessThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testOneDeadBranchThreeBranches1 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 11.
	
	self assert: machineSimulator receiverRegisterValue equals: 6
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testOneDeadBranchThreeBranches2 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 104
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testOneDeadBranchThreeBranches3 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 116
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testPassingSuccess [

	self compileDruidPrimitive: #passingSuccess.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testPrimitiveWithDeadCodeGoesThroughFirstCase [

	self compileDruidPrimitive: #primitiveWithDeadCode.
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-deadcode' }
DRPrimitiveCompilationTest >> testPrimitiveWithDeadCodeGoesThroughSecondCase [

	self compileDruidPrimitive: #primitiveWithDeadCode.
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 55
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSmallerOrEqualsThan [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSmallerOrEqualsThanEquals [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSmallerOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSmallerThan [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSmallerThanInverted [

	self compileDruidPrimitive: #primitiveLessThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testSubtractionIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveSubWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 32)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveCompilationTest >> testSubtractionIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveSubWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory minSmallInteger) withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0708
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At2 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 1.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0607
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At3 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 2.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0506
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At4 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 3.
	memory storePointer: 0 ofObject: object withValue: 16r0405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0405
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At5 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 4.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0304
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At6 [

	| object pointerToFirstSlot |

	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 5.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0203
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint16At7 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 6.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0102
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint32At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r05060708
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint32At2 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 1.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r04050607
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint32At3 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 2.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r03040506
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint32At4 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 3.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r02030405
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint32At5 [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 4.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r01020304
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint64At [

	| object pointerToFirstSlot |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint64At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r0102030405060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0102030405060708
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint64AtPut [

	| object pointerToFirstSlot result |
	
	"Only for 64 bit platforms"
	self wordSize = 4 ifTrue: [ self skip ].
	
	self compileDruidPrimitive: #primitiveUint64AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot withArguments: { 16r0102030405060708 }.
	
	result := memory fetchPointer: 0 ofObject: object.
	self assert: result equals: 16r0102030405060708
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint8At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r000000000000CAFE.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rFE
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint8At2 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 1.
	memory storePointer: 0 ofObject: object withValue: 16r000000000000CAFE.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rCA
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint8At3 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 2.
	memory storePointer: 0 ofObject: object withValue: 16r00000000BABE0000.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rBE
]

{ #category : #'test-memory access' }
DRPrimitiveCompilationTest >> testUint8At4 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 3.
	memory storePointer: 0 ofObject: object withValue: 16r00000000BABE0000.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rBA
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testWhileTrue [

	self compileDruidPrimitive: #primitiveLoopIncrementing.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 15
]

{ #category : #'tests-control flow' }
DRPrimitiveCompilationTest >> testWhileTrueWithGlobalState [

	self compileDruidPrimitive: #primitiveLoopIncrementingGlobalState.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 15
]
