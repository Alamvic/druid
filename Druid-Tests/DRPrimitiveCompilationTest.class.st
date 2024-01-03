Class {
	#name : #DRPrimitiveCompilationTest,
	#superclass : #DRDruidTest,
	#instVars : [
		'stopAddress',
		'cogitStyle',
		'compilerClass'
	],
	#category : #'Druid-Tests'
}

{ #category : #'building suites' }
DRPrimitiveCompilationTest class >> cogitStyleParameter [
	"Two ways to compile code with different calling conventions: simple stack vs stack to register mapping"

	^ ParametrizedTestMatrix new
		addCase: { #cogitStyle -> #beForSimpleStack. #compilerClass -> SimpleDruidTestRTLCompiler};
		addCase: { #cogitStyle -> #beForStackToRegisterMapping. #compilerClass -> DruidTestRTLCompiler};
		yourself
]

{ #category : #'building suites' }
DRPrimitiveCompilationTest class >> testParameters [ 

	^ super testParameters * self cogitStyleParameter
]

{ #category : #accessing }
DRPrimitiveCompilationTest >> cogitStyle: aSymbol [

	cogitStyle := aSymbol
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidDeferredInlinePrimitive: selector [

	self compileDruidPrimitive: selector with: [ :compiler | 
		compiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new ]
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
		perform: cogitStyle;
		compilerClass: compilerClass.
		
	aBlock value: compiler.
	compiler compile.

	"Then generate the machine code for that method"
	cogInitialAddress := self compile: [
		cogit methodOrBlockNumArgs: (compiler numberOfArguments). "Hack"
		cogit perform: generatorSelector.
		stopInstruction := cogit Stop
	] bytecodes: 100.
	stopAddress := stopInstruction address
]

{ #category : #accessing }
DRPrimitiveCompilationTest >> compilerClass [

	^ compilerClass
]

{ #category : #accessing }
DRPrimitiveCompilationTest >> compilerClass: anObject [

	compilerClass := anObject
]

{ #category : #'helpers - execution' }
DRPrimitiveCompilationTest >> executeUntilStopPrimitiveWithReceiver: receiverOop [
	
	^ self executeUntilStopPrimitiveWithReceiver: receiverOop withArguments: { }
]

{ #category : #'helpers - execution' }
DRPrimitiveCompilationTest >> executeUntilStopPrimitiveWithReceiver: receiverOop withArguments: aCollection [
	
	"Simulate a primitive execution having an object as receiver and a single argument
	  - the receiver goes to the receiver register
	  - the argument should be pushed to the stack
	If we are in a system without a link register, we need to push the caller IP to the stack to simulate a call"
	
	self prepareStackForSendReceiver: receiverOop arguments: aCollection.
	self runFrom: cogInitialAddress until: stopAddress.
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> jitCompilerClass [

	^ compilerClass
]
