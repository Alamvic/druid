Class {
	#name : #DRPrimitiveCompilationTest,
	#superclass : #VMJittedGeneralPrimitiveTest,
	#category : #'Druid-Tests'
}

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector [
	
	| generatorSelector |
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
	].
	
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> interpreterClass [ 

	^ DruidTestInterpreter
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> jitCompilerClass [

	^ DruidTestRTLCompiler
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> jitOptions [

	^ super jitOptions
		  at: #bytecodeTableInitializer put: #initializeBytecodeTableForSistaV1;
		  yourself
]

{ #category : #running }
DRPrimitiveCompilationTest >> setUp [

	super setUp.

	cogit objectRepresentation: cogit.
	cogit methodZone
		setInterpreter: interpreter
		objectRepresentation: cogit
		cogit: cogit.
		
	initialAddress := #invalid "will be set by test"
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
DRPrimitiveCompilationTest >> testCompileEmptyPrimitiveWithoutArgumentsReturnsReceiver [

	| receiver |
	self compileDruidPrimitive: #emptyPrimitiveWithArguments.
	
	receiver := memory integerObjectOf: 43.
	self executePrimitiveWithReceiver: receiver withArgument: memory trueObject.
	
	"An empty primitive does nothing and just returns, the return value is the receiver"
	self assert: machineSimulator receiverRegisterValue equals: receiver.
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

{ #category : #'tests-comparisons' }
DRPrimitiveCompilationTest >> testSubstraction [

	self compileDruidPrimitive: #primitiveDecrement.
	
	self executePrimitiveWithReceiver: 42.
	
	self assert: machineSimulator receiverRegisterValue equals: 41
]
