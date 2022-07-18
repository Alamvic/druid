Class {
	#name : #DRPrimitiveScenarioCompilationTest,
	#superclass : #DRPrimitiveCompilationTest,
	#category : #'Druid-Tests'
}

{ #category : #'tests-methodactivations' }
DRPrimitiveScenarioCompilationTest >> testAccessTemporaryAfterCall [

	| receiver |
	self compileDruidPrimitive: #primitiveAccessTempAfterCall.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testAdditionIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveSumWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 52)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testAdditionIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveSumWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory maxSmallInteger) withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testAndFirstFalseSecondFalse [

	self compileDruidPrimitive: #primitiveAnd.
	
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 10
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testAndFirstTrueSecondFalse [

	self compileDruidPrimitive: #primitiveAnd.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 10
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testAndFirstTrueSecondTrue [

	self compileDruidPrimitive: #primitiveAnd.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testAnyMaskFalse [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testAnyMaskFalseInverted [

	self compileDruidPrimitive: #primitiveConditionWithAnyMaskInverted.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testAnyMaskTrue [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 1

]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testAnyMaskTrueInverted [

	self compileDruidPrimitive: #primitiveConditionWithAnyMaskInverted.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 1

]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testArithmeticBitShiftRight [

	self compileDruidPrimitive: #primitiveArithmeticBitShiftRight.

	self executePrimitiveWithReceiver: 2r10 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r1
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testAssertIsIgnored [

	self compileDruidPrimitive: #primitiveAssertIsIgnored.

	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 17

]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testBitXor [

	self compileDruidPrimitive: #primitiveBitXor.
	
	self executePrimitiveWithReceiver: 2r110 withArguments: { 2r010 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r100
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testBranchingWithAssigmentsFalse [

	self compileDruidPrimitive: #primitiveBranchingWithAssigments.
	
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testBranchingWithAssigmentsTrue [

	self compileDruidPrimitive: #primitiveBranchingWithAssigments.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCascade [

	| object pointerToFirstSlot result |
	self compileDruidPrimitive: #primitiveCascadedUint16AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 0.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	result := memory fetchShort16: 0 ofObject: object.
	self assert: result equals: 1.
	
	result := memory fetchShort16: 1 ofObject: object.
	self assert: result equals: 2
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testCaseOfOtherwiseCaseOne [

	self compileDruidPrimitive: #primitiveCaseOfOtherwiseValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testCaseOfOtherwiseCaseOtherwise [

	self compileDruidPrimitive: #primitiveCaseOfOtherwiseValue.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 77
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testCaseOfOtherwiseCaseTwo [

	self compileDruidPrimitive: #primitiveCaseOfOtherwiseValue.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testClassVariables [

	self compileDruidPrimitive: #primitiveClassVariable.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: ClassFloatCompactIndex
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testClassVariablesWithControlFlow [

	self compileDruidPrimitive: #primitiveClassVariableWithBranch.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: ClassFloatCompactIndex + 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testCompareObjectReferenceFalse [

	self compileDruidPrimitive: #primitiveConditionWithObjectReference.
	
	self executePrimitiveWithReceiver: (memory trueObject + 1).
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testCompareObjectReferenceTrue [

	self compileDruidPrimitive: #primitiveConditionWithObjectReference.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testCompareRefenceValueFalse [

	self compileDruidPrimitive: #primitiveGreaterThanReferenceValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testCompareRefenceValueTrue [

	self compileDruidPrimitive: #primitiveGreaterThanReferenceValue.
	
	self executePrimitiveWithReceiver: memory trueObject + 1.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompileEmptyPrimitiveWithArgumentsReturnsReceiver [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitive.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: receiver.
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompileEmptyPrimitiveWithoutArgumentsReturnsTopOfTheStack [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitiveWithArguments.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver withArgument: memory trueObject.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: receiver
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompileNonCompiledCaseExitsWithFailure [

	| receiver |
	self compileDruidPrimitive: #primitiveWithNonCompilableBranch.
	
	receiver := memory integerObjectOf: -1.
	
	self executeUntilStopPrimitiveWithReceiver: receiver.
	
	"Should stop at the stop and not timeout"
	self assert: machineSimulator instructionPointerRegisterValue equals: stopAddress
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompileNonCompiledCaseExitsWithSuccess [

	| receiver |
	self compileDruidPrimitive: #primitiveWithNonCompilableBranch.
	
	receiver := memory integerObjectOf: 1.
	
	self executePrimitiveWithReceiver: receiver.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingBlock [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReturningConstant.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingBlockReadingExternalTemp [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReadingExternalTemp.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingBlockWithArgument [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockReturningArgument.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingBlockWithNonLocalReturn [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockWithNonLocalReturn.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingBlockWritingExternalTemp [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingBlockWritingExternalTemp.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'test-blocks' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingEmptyBlock [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingEmptyBlock.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.

	self assert: machineSimulator receiverRegisterValue equals: 18
]

{ #category : #'tests-methodactivations' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingMethod [

	| receiver |
	self compileDruidPrimitive: #primitiveCallingMethodReturningConstant.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 73
]

{ #category : #'tests-methodactivations' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingMethodWithEarlyReturnCaseFalse [

	self compileDruidPrimitive: #primitiveCallingMethodWithEarlyReturn.
	
	self executePrimitiveWithReceiver: 1237123.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-methodactivations' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveCallingMethodWithEarlyReturnCaseTrue [

	self compileDruidPrimitive: #primitiveCallingMethodWithEarlyReturn.
	
	self executePrimitiveWithReceiver: 0.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveReturningConstant [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnOne.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveReturningFalseObjectReference [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnFalse.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	self assert: machineSimulator receiverRegisterValue equals: memory falseObject
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testCompilePrimitiveReturningObjectReference [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnTrue.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #'tests-stack' }
DRPrimitiveScenarioCompilationTest >> testCompiledPrimitiveInitializesCorrectlyTheSimulatedStack [

	| receiver |
	self compileDruidPrimitive: #primitiveReturnOneWithArguments.
	
	receiver := memory integerObjectOf: 43.
	self pushAddress: 55.
	self executePrimitiveWithReceiver: receiver withArguments: { 1. 2 }.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-stack' }
DRPrimitiveScenarioCompilationTest >> testCompiledPrimitivePreservesStack [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitive.
	
	receiver := memory integerObjectOf: 43.
	self pushAddress: 55.
	self executePrimitiveWithReceiver: receiver.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: self popAddress equals: 55
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDecrementConstantArgument [

	self compileDruidPrimitive: #primitiveDecrementConstantArgument.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 41
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDecrementConstantReceiver [

	self compileDruidPrimitive: #primitiveDecrementConstantReceiver.

	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 37
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDivision [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDivisionByConstant [

	self compileDruidPrimitive: #primitiveDivideByConstant.

	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDivisionByZero [

	<expectedFailure>

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 0 }.
	
	self flag: #todo. "Fix simulator"
	self assert: machineSimulator overflow "Replace with expected assertion"
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDivisionNotExact [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testDivisionNotExactNegative [

	self compileDruidPrimitive: #primitiveDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { -3 }.
	
	self assert: (machineSimulator receiverRegisterValue signedIntFromShort) equals: -1.   
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testEqualsThan [

	self compileDruidPrimitive: #primitiveEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanFail [

	self compileDruidPrimitive: #primitiveEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanFloatsEquals [

	self compileDruidPrimitive: #primitiveEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanFloatsGreater [

	self compileDruidPrimitive: #primitiveEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanFloatsLess [

	self compileDruidPrimitive: #primitiveEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanInverted [

	self compileDruidPrimitive: #primitiveEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanInvertedFail [

	self compileDruidPrimitive: #primitiveEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanValueFalse [

	self compileDruidPrimitive: #primitiveEqualsThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testEqualsThanValueTrue [

	self compileDruidPrimitive: #primitiveEqualsThanValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testFailingPrimitive2ExecutesUntilStop [

	self compileDruidPrimitive: #failingPrimitive.
	
	self executeUntilStopPrimitiveWithReceiver: 1
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testFailingPrimitiveWithArgExecutesUntilStop [

	self compileDruidPrimitive: #failingPrimitiveWithArg.
	
	self executeUntilStopPrimitiveWithReceiver: 1 withArguments: { 42 }
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testFailingSuccessExecutesUntilStop [

	self compileDruidPrimitive: #failingSuccess.
	
	self executeUntilStopPrimitiveWithReceiver: 1
]

{ #category : #'tests-booleans' }
DRPrimitiveScenarioCompilationTest >> testFalseAsInteger [

	self compileDruidPrimitive: #primitiveFalseAsInteger.
	
	self executePrimitiveWithReceiver: 17.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-booleans' }
DRPrimitiveScenarioCompilationTest >> testFalseNot [

	self compileDruidPrimitive: #primitiveFalseNot.
	
	self executePrimitiveWithReceiver: 17.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testFloat64At [

	| object pointerToFirstSlot |
	
	self compileDruidPrimitive: #primitiveFloat64At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	memory storeFloatAtPointer: pointerToFirstSlot from: 1234.0.
	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: (Float fromIEEE64BitWord: machineSimulator receiverRegisterValue) equals: 1234.0
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testFloat64AtPut [

	| object pointerToFirstSlot result |
	
	self compileDruidPrimitive: #primitiveFloat64AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot withArguments: { 1234 }.
	
	result := memory fetchPointer: 0 ofObject: object.
	self assert: (Float fromIEEE64BitWord: result) equals: 1234.0
]

{ #category : #'tests-arithmetic-floats' }
DRPrimitiveScenarioCompilationTest >> testFloatAdd [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveFloatAdd.
	self
		executePrimitiveWithReceiver: 17
		withArguments: { 21 }.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: (Float fromIEEE64Bit: floatRawValue) equals: 38
]

{ #category : #'tests-arithmetic-floats' }
DRPrimitiveScenarioCompilationTest >> testFloatDivide [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveFloatDivide.
	self
		executePrimitiveWithReceiver: 17
		withArguments: { 21 }.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: (Float fromIEEE64Bit: floatRawValue) equals: 21.0 / 17.0
]

{ #category : #'tests-arithmetic-floats' }
DRPrimitiveScenarioCompilationTest >> testFloatMultiply [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveFloatMultiply.
	self
		executePrimitiveWithReceiver: 17
		withArguments: { 21 }.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: (Float fromIEEE64Bit: floatRawValue) equals: 21.0 * 17.0
]

{ #category : #'tests-arithmetic-floats' }
DRPrimitiveScenarioCompilationTest >> testFloatSubtract [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveFloatSubtract.
	self
		executePrimitiveWithReceiver: 17
		withArguments: { 21 }.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: (Float fromIEEE64Bit: floatRawValue) equals: 4.0
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testFourDeadBranchesFourBranches1 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 11.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testFourDeadBranchesFourBranches2 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 119
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testFourDeadBranchesFourBranches3 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 7.
	
	self assert: machineSimulator receiverRegisterValue equals: 146
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testFourDeadBranchesFourBranches4 [

	self compileDruidPrimitive: #primitiveDNA.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 158
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualThanValueFalse [

	self compileDruidPrimitive: #primitiveGreaterOrEqualThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualThanValueTrue [

	self compileDruidPrimitive: #primitiveGreaterOrEqualThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThan [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanEquals [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanEqualsInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanFloatsEquals [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanFloatsGreater [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanFloatsLess [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterThan [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterThanFloatsEquals [

	self compileDruidPrimitive: #primitiveGreaterThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterThanFloatsGreater [

	self compileDruidPrimitive: #primitiveGreaterThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testGreaterThanFloatsLess [

	self compileDruidPrimitive: #primitiveGreaterThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testGreaterThanInverted [

	self compileDruidPrimitive: #primitiveGreaterThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfFalseIfTrueReturningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfFalseIfTrueReturningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfFalseIfTrueReturningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfFalseIfTrueReturningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNilNotNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNilIfNotNilStatement.
	
	"If bigger than 0, it will see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNilNotNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNilIfNotNilStatement.
	
	"If smaller than 0, it will not see nil and set 42"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilArgumentIfNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatementWithArgument.
	
	"If bigger than 0, it will see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilArgumentIfNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatementWithArgument.
	
	"If smaller than 0, it will not see nil and set 42"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilIfNilStatementWithNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilIfNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilIfNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilStatementWithNil [

	self compileDruidPrimitive: #primitiveDeadBranchWithError.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 17
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfNotNilStatementWithNotNil [

	self compileDruidPrimitive: #primitiveWithIfNotNilStatement.
	
	"If smaller than 0, it will not see nil and set 17"
	self executePrimitiveWithReceiver: -1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueIfFalseAssigningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseAssigningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueIfFalseAssigningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseAssigningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueIfFalseReturningValueCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseReturningValue.
	
	self executePrimitiveWithReceiver: 9898.
	
	self assert: machineSimulator receiverRegisterValue equals: 57
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueIfFalseReturningValueCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueIfFalseReturningValue.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueStatementCaseFalse [

	self compileDruidPrimitive: #primitiveIfTrueStatement.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 10
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testIfTrueStatementCaseTrue [

	self compileDruidPrimitive: #primitiveIfTrueStatement.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testImplicitBitShiftLeft [

	self compileDruidPrimitive: #primitiveBitShift.

	self executePrimitiveWithReceiver: 2r10 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r100
]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testImplicitBitShiftRight [

	self compileDruidPrimitive: #primitiveBitShift.

	self executePrimitiveWithReceiver: 2r10 withArguments: { -1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r1
]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testImplicitConstantArgumentBitShiftLeft [

	self compileDruidPrimitive: #primitiveImplicitArgumentBitShiftLeft.

	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r100
]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testImplicitConstantBitShiftLeft [

	self compileDruidPrimitive: #primitiveImplicitBitShiftLeft.

	self executePrimitiveWithReceiver: 2r10.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r100
]

{ #category : #'tests-bitwise' }
DRPrimitiveScenarioCompilationTest >> testImplicitConstantBitShiftRight [

	self compileDruidPrimitive: #primitiveImplicitBitShiftRight.

	self executePrimitiveWithReceiver: 2r10.
	
	self assert: machineSimulator receiverRegisterValue equals: 2r1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIncrementConstantArgument [

	self compileDruidPrimitive: #primitiveIncrementConstantArgument.

	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIncrementConstantReceiver [

	self compileDruidPrimitive: #primitiveIncrementConstantReceiver.

	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #'tests-types' }
DRPrimitiveScenarioCompilationTest >> testIntegerAsFloat [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveIntegerAsFloat.
	self executePrimitiveWithReceiver: 17.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: (Float fromIEEE64Bit: floatRawValue) equals: 17.0
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIntegerDivision [

	self compileDruidPrimitive: #primitiveIntegerDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIntegerDivisionByConstant [

	self compileDruidPrimitive: #primitiveIntegerDivideByConstant.

	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 21
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIntegerDivisionByZero [

	<expectedFailure>

	self compileDruidPrimitive: #primitiveIntegerDivide.

	self executePrimitiveWithReceiver: 42 withArguments: { 0 }.
	
	self flag: #todo. "Fix simulator"
	self assert: machineSimulator overflow "Replace with expected assertion"
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIntegerDivisionNotExact [

	self compileDruidPrimitive: #primitiveIntegerDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testIntegerDivisionNotExactNegative [

	self compileDruidPrimitive: #primitiveIntegerDivide.

	self executePrimitiveWithReceiver: 5 withArguments: { -3 }.
	
	self assert: (machineSimulator receiverRegisterValue signedIntFromShort) equals: -1.   
]

{ #category : #'tests-types' }
DRPrimitiveScenarioCompilationTest >> testIntegerRawBitsAsFloat [

	| floatRawValue |
	self compileDruidPrimitive: #primitiveIntegerRawBitsAsFloat.
	self executePrimitiveWithReceiver: 17.0 asIEEE64BitWord.
	
	floatRawValue := machineSimulator receiverRegisterValue.
	
	self assert: floatRawValue equals: 17
]

{ #category : #'tests-builtins' }
DRPrimitiveScenarioCompilationTest >> testIsIntegerObjectWithIntegerObject [

	self compileDruidPrimitive: #primitiveIsIntegerObject.
	self executePrimitiveWithReceiver: (memory integerObjectOf: 0).
	
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject
]

{ #category : #'tests-builtins' }
DRPrimitiveScenarioCompilationTest >> testIsIntegerObjectWithNonIntegerObject [

	self compileDruidPrimitive: #primitiveIsIntegerObject.
	self executePrimitiveWithReceiver: 0 "No tag".
	
	self assert: machineSimulator receiverRegisterValue equals: memory falseObject
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testJitCompileTimeExpression [

	memory trueObject: 17.
	self compileDruidPrimitive: #primitiveJITCompileTimeExpression.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 22 "17 + 5"
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testLessOrEqualThanValueFalse [

	self compileDruidPrimitive: #primitiveLessOrEqualThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testLessOrEqualThanValueTrue [

	self compileDruidPrimitive: #primitiveLessOrEqualThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessOrEqualsThanFloatsEquals [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessOrEqualsThanFloatsGreater [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessOrEqualsThanFloatsLess [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessThanFloatsEquals [

	self compileDruidPrimitive: #primitiveLessThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessThanFloatsGreater [

	self compileDruidPrimitive: #primitiveLessThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testLessThanFloatsLess [

	self compileDruidPrimitive: #primitiveLessThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testLessThanValueFalse [

	self compileDruidPrimitive: #primitiveLessThanValue.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: false asBit
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testLessThanValueTrue [

	self compileDruidPrimitive: #primitiveLessThanValue.
	
	self executePrimitiveWithReceiver: -10.
	
	self assert: machineSimulator receiverRegisterValue equals: true asBit
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testMod [

	self compileDruidPrimitive: #primitiveModByConstant.

	self executePrimitiveWithReceiver: 43.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testModNegative [

	self compileDruidPrimitive: #primitiveMod.

	self executePrimitiveWithReceiver: 43 withArguments: { -2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testMultiplicationIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveMultiplyWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
		
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 420)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testMultiplicationIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveMultiplyWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory maxSmallInteger) withArguments: { memory integerObjectOf: 2 }.

	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testMultiplyConstantArgument [

	self compileDruidPrimitive: #primitiveMultiplyConstantArgument.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 84
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testMultiplyConstantReceiver [

	self compileDruidPrimitive: #primitiveMultiplyConstantReceiver.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 84
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testNegated [

	self compileDruidPrimitive: #primitiveNegated.
	
	self executePrimitiveWithReceiver: 5.

	self assert: (machineSimulator receiverRegisterValue signedIntFromShort) equals: -5
]

{ #category : #'tests-arithmetic' }
DRPrimitiveScenarioCompilationTest >> testNegatedNegative [

	self compileDruidPrimitive: #primitiveNegated.
	
	self executePrimitiveWithReceiver: -5.

	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThan [

	self compileDruidPrimitive: #primitiveNotEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanFail [

	self compileDruidPrimitive: #primitiveNotEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanFloatsEquals [

	self compileDruidPrimitive: #primitiveNotEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 2 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanFloatsGreater [

	self compileDruidPrimitive: #primitiveNotEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 3 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons-floats' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanFloatsLess [

	self compileDruidPrimitive: #primitiveNotEqualsThanFloats.
	
	self executePrimitiveWithReceiver: 2 withArguments: { 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanInverted [

	self compileDruidPrimitive: #primitiveNotEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotEqualsThanInvertedFail [

	self compileDruidPrimitive: #primitiveNotEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotGreaterOrEqualsThan [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThan.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotGreaterOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveGreaterOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotGreaterThan [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotGreaterThanEquals [

	self compileDruidPrimitive: #primitiveGreaterThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotGreaterThanInverted [

	self compileDruidPrimitive: #primitiveGreaterThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotSmallerOrEqualsThan [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 50.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotSmallerOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotSmallerThan [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotSmallerThanEquals [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testNotSmallerThanInverted [

	self compileDruidPrimitive: #primitiveLessThanInverted.
	
	self executePrimitiveWithReceiver: 2.
	
	self assert: machineSimulator receiverRegisterValue equals: 11
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testOneDeadBranchThreeBranches1 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 11.
	
	self assert: machineSimulator receiverRegisterValue equals: 6
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testOneDeadBranchThreeBranches2 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 104
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testOneDeadBranchThreeBranches3 [

	self compileDruidPrimitive: #primitiveSandclock.
	
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 116
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testOrFirstFalseSecondFalse [

	self compileDruidPrimitive: #primitiveOr.
	
	self executePrimitiveWithReceiver: 7.
	
	self assert: machineSimulator receiverRegisterValue equals: 10
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testOrFirstFalseSecondTrue [

	self compileDruidPrimitive: #primitiveOr.
	
	self executePrimitiveWithReceiver: 12.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-comparisons value' }
DRPrimitiveScenarioCompilationTest >> testOrFirstTrueSecondFalse [

	self compileDruidPrimitive: #primitiveOr.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testPassingSuccess [

	self compileDruidPrimitive: #passingSuccess.
	
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 1
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testPrimitiveAdd [

	cogit receiverTags: memory smallIntegerTag.

	self compileDruidPrimitive: #primitiveAdd.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 52)
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testPrimitiveWithDeadCodeGoesThroughFirstCase [

	self compileDruidPrimitive: #primitiveWithDeadCode.
	self executePrimitiveWithReceiver: 0.
	
	self assert: machineSimulator receiverRegisterValue equals: 42
]

{ #category : #'tests-deadcode' }
DRPrimitiveScenarioCompilationTest >> testPrimitiveWithDeadCodeGoesThroughSecondCase [

	self compileDruidPrimitive: #primitiveWithDeadCode.
	self executePrimitiveWithReceiver: 1.
	
	self assert: machineSimulator receiverRegisterValue equals: 55
]

{ #category : #'tests-basic' }
DRPrimitiveScenarioCompilationTest >> testSequentialExitPointRemovesEntireExecutionBranch [

	self compileDruidPrimitive: #primitiveWithSequentialExitPoint.
	
	self executeUntilStopPrimitiveWithReceiver: 1.
	
	"Should stop at the stop and not timeout"
	self assert: machineSimulator instructionPointerRegisterValue equals: stopAddress
]

{ #category : #'tests-types' }
DRPrimitiveScenarioCompilationTest >> testSizeOfUsqInt [

	self compileDruidPrimitive: #primitiveSizeofusqInt.
	
	self executePrimitiveWithReceiver: 17.
	
	self assert: machineSimulator receiverRegisterValue equals: memory bytesPerOop
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testSmallerOrEqualsThan [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testSmallerOrEqualsThanEquals [

	self compileDruidPrimitive: #primitiveLessOrEqualsThan.
	
	self executePrimitiveWithReceiver: 10.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testSmallerOrEqualsThanInverted [

	self compileDruidPrimitive: #primitiveLessOrEqualsThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testSmallerThan [

	self compileDruidPrimitive: #primitiveLessThan.
	
	self executePrimitiveWithReceiver: 9.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-comparisons' }
DRPrimitiveScenarioCompilationTest >> testSmallerThanInverted [

	self compileDruidPrimitive: #primitiveLessThanInverted.
	
	self executePrimitiveWithReceiver: 20.
	
	self assert: machineSimulator receiverRegisterValue equals: 5
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testSubtractionIntrinsecWithNoOverflow [

	self compileDruidPrimitive: #primitiveSubWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: 42) withArguments: { memory integerObjectOf: 10 }.
	
	self assert: machineSimulator receiverRegisterValue equals: (memory integerObjectOf: 32)
]

{ #category : #'tests-arithmetic-smallIntegers' }
DRPrimitiveScenarioCompilationTest >> testSubtractionIntrinsecWithOverflow [

	self compileDruidPrimitive: #primitiveSubWithOverflow.

	self executePrimitiveWithReceiver: (memory integerObjectOf: memory minSmallInteger) withArguments: { memory integerObjectOf: 1 }.
	
	self assert: machineSimulator receiverRegisterValue equals: 99
]

{ #category : #'tests-booleans' }
DRPrimitiveScenarioCompilationTest >> testTrueNot [

	self compileDruidPrimitive: #primitiveTrueNot.
	
	self executePrimitiveWithReceiver: 17.
	
	self assert: machineSimulator receiverRegisterValue equals: 0
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0708
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16At2 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 1.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0607
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16At3 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 2.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0506
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16At4 [

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
DRPrimitiveScenarioCompilationTest >> testUint16At5 [

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
DRPrimitiveScenarioCompilationTest >> testUint16At6 [

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
DRPrimitiveScenarioCompilationTest >> testUint16At7 [

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
DRPrimitiveScenarioCompilationTest >> testUint16AtPut [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint16AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot withArguments: { 16r0102 }.
	
	result := memory fetchShort16: 0 ofObject: object.
	self assert: result equals: 16r0102
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16AtPut1 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint16AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 2 withArguments: { 16r0102 }.
	
	result := memory fetchShort16: 1 ofObject: object.
	self assert: result equals: 16r0102
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16AtPut2 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint16AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 4 withArguments: { 16r0102 }.
	
	result := memory fetchShort16: 2 ofObject: object.
	self assert: result equals: 16r0102
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16AtPut4 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint16AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 6 withArguments: { 16r0102 }.
	
	result := memory fetchShort16: 3 ofObject: object.
	self assert: result equals: 16r0102
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint16AtWithSameOperandRegister [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveNextUint16At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r0607
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint32At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint32At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r05060708.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16r05060708
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint32At2 [

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
DRPrimitiveScenarioCompilationTest >> testUint32At3 [

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
DRPrimitiveScenarioCompilationTest >> testUint32At4 [

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
DRPrimitiveScenarioCompilationTest >> testUint32At5 [

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
DRPrimitiveScenarioCompilationTest >> testUint32AtPut [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint32AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot withArguments: { 16r01020304 }.
	
	result := memory fetchLong32: 0 ofObject: object.
	self assert: result equals: 16r01020304
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint32AtPut2 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint32AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 4 withArguments: { 16r01020304 }.
	
	result := memory fetchLong32: 1 ofObject: object.
	self assert: result equals: 16r01020304
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint64At [

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
DRPrimitiveScenarioCompilationTest >> testUint64AtPut [

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
DRPrimitiveScenarioCompilationTest >> testUint8At [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.
	memory storePointer: 0 ofObject: object withValue: 16r000000000000CAFE.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rFE
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8At2 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 1.
	memory storePointer: 0 ofObject: object withValue: 16r000000000000CAFE.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rCA
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8At3 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 2.
	memory storePointer: 0 ofObject: object withValue: 16r00000000BABE0000.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rBE
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8At4 [

	| object pointerToFirstSlot |
	self compileDruidPrimitive: #primitiveUint8At.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize + 3.
	memory storePointer: 0 ofObject: object withValue: 16r00000000BABE0000.

	self executePrimitiveWithReceiver: pointerToFirstSlot.
	
	self assert: machineSimulator receiverRegisterValue equals: 16rBA
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot withArguments: { 16rF1 }.
	
	result := memory fetchByte: 0 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut1 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 1 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 1 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut2 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 2 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 2 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut3 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 3 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 3 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut4 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 4 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 4 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut5 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 5 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 5 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut6 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 6 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 6 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'test-memory access' }
DRPrimitiveScenarioCompilationTest >> testUint8AtPut7 [

	| object pointerToFirstSlot result |
		
	self compileDruidPrimitive: #primitiveUint8AtPut.
	
	object := self newObjectWithSlots: 1.
	pointerToFirstSlot := object + memory baseHeaderSize.

	self executePrimitiveWithReceiver: pointerToFirstSlot + 7 withArguments: { 16rF1 }.
	
	result := memory fetchByte: 7 ofObject: object.
	self assert: result equals: 16rF1
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testWhileTrue [

	self compileDruidPrimitive: #primitiveLoopIncrementing.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 15
]

{ #category : #'tests-control flow' }
DRPrimitiveScenarioCompilationTest >> testWhileTrueWithGlobalState [

	self compileDruidPrimitive: #primitiveLoopIncrementingGlobalState.
	
	self executePrimitiveWithReceiver: 5.
	
	self assert: machineSimulator receiverRegisterValue equals: 15
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testifZeroFailPrimitiveFail [

	self compileDruidPrimitive: #ifZeroFailPrimitive.
	
	self executeUntilStopPrimitiveWithReceiver: 0 withArguments: { 0 }
]

{ #category : #'tests-failing-primitive' }
DRPrimitiveScenarioCompilationTest >> testifZeroFailPrimitiveSuccess [

	self compileDruidPrimitive: #ifZeroFailPrimitive.

	self executePrimitiveWithReceiver: 1 withArguments: { 0 }.

	self assert: machineSimulator receiverRegisterValue equals: 42
]
