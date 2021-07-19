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

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAddition [

	self compileDruidPrimitive: #primitiveIncrement.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 43
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskFalse [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 2
]

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testAnyMaskTrue [

	self compileDruidPrimitive: #primitiveConditionWithAnyMask.
	
	self executePrimitiveWithReceiver: 37.
	
	self assert: machineSimulator receiverRegisterValue equals: 1

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
	self assert: machineSimulator receiverRegisterValue equals: memory trueObject.
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

{ #category : #'tests-failing-primitive' }
DRPrimitiveCompilationTest >> testFailingPrimitiveExecutesUntilStop [

	self compileDruidPrimitive: #failingPrimitive.
	
	self executeUntilStopPrimitiveWithReceiver: 1
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
DRPrimitiveCompilationTest >> testSubstraction [

	self compileDruidPrimitive: #primitiveDecrement.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 41
]
