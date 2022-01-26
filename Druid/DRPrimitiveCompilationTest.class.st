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
		cogit perform: generatorSelector.
		stopInstruction := cogit Stop
	].
	stopAddress := stopInstruction address
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> druidIROfPrimitive: aSelector [
	
	^ DRPrimitiveCompilerCompiler new
		primitiveName: aSelector;
		interpreter: self interpreter;
		generateDruidIR
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
DRPrimitiveCompilationTest >> testAssertExecutesUntilStop [

	self compileDruidPrimitive: #failingAssert.
	
	self executeUntilStopPrimitiveWithReceiver: 1
]

{ #category : #'tests-iftrueiffalse' }
DRPrimitiveCompilationTest >> testCompileConditionalControlFlowAssigningValueCaseFalse [

	self compileDruidPrimitive: #primitiveWithIfAssigningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-iftrueiffalse' }
DRPrimitiveCompilationTest >> testCompileConditionalControlFlowAssigningValueCaseTrue [

	self compileDruidPrimitive: #primitiveWithIfAssigningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-iftrueiffalse' }
DRPrimitiveCompilationTest >> testCompileConditionalControlFlowCaseFalse [

	self compileDruidPrimitive: #primitiveWithIfReturningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-iftrueiffalse' }
DRPrimitiveCompilationTest >> testCompileConditionalControlFlowCaseTrue [

	self compileDruidPrimitive: #primitiveWithIfReturningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
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
	self assert: self popAddress equals: 55
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

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	
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

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testPassingAssert [

	self flag: #FIXME. "FAILING! see interpretSignalWith:"
	self compileDruidPrimitive: #passingAssert.
	
	self executeUntilStopPrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
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
