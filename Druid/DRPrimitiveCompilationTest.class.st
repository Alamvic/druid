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
	
	^ self compileDruidPrimitive: aSelector with: [ :compiler | "nothing" ]
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector with: aBlock [
	
	| generatorSelector stopInstruction compiler |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.

	"First generate druid code"
	compiler := DRPrimitiveCompilerCompiler new
		primitiveName: aSelector;
		interpreter: self interpreter;
		primitiveCompilerName: generatorSelector;
		compilerClass: DruidTestRTLCompiler.
		
	aBlock value: compiler.
	compiler compile.

	"Then generate the machine code for that method"
	initialAddress := self compile: [
		cogit methodOrBlockNumArgs: 2. "Hack"
		cogit perform: generatorSelector.
		stopInstruction := cogit Stop
	] bytecodes: 100.
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
