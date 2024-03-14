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
		addCase: { #cogitStyle -> #beForStackToRegisterMapping. #compilerClass -> DruidTestRTLCompiler};
		addCase: { #cogitStyle -> #beForSimpleStack. #compilerClass -> SimpleDruidTestRTLCompiler};
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
DRPrimitiveCompilationTest >> compileCogitSelector: aSelector numArgs: argumentCount [

	| stopInstruction |
	cogInitialAddress := self
		                     compile: [
			                     cogit methodOrBlockNumArgs: argumentCount.
			                     cogit perform: aSelector.
			                     stopInstruction := cogit Stop ]
		                     bytecodes: 100.
	stopAddress := stopInstruction address
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidDeferredInlinePrimitive: selector [

	self compileDruidPrimitive: selector with: [ :compiler | 
		compiler irGenerator: DRPrimitiveIRGeneratorDeferredInline new ]
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector [
	
	| compiler |
	compiler := self compileDruidPrimitiveGenerator: aSelector with: [ :c | "nothing" ].

	self compileCogitSelector: compiler primitiveCompilerName numArgs: compiler numberOfArguments
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector numArgs: argumentCount [
	
	| compiler |
	compiler := self compileDruidPrimitiveGenerator: aSelector with: [ :c | "nothing" ].
	
	self compileCogitSelector: compiler primitiveCompilerName numArgs: argumentCount
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector numArgs: argumentCount with: aBlock [
	
	| generatorSelector stopInstruction compiler |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.

	"First generate druid code"
	compiler := DRPrimitiveCompilerCompiler new
		primitiveName: aSelector;
		interpreter: self interpreter;
		primitiveCompilerName: generatorSelector;
		perform: cogitStyle;
		configureForCompilerClass: compilerClass.

	aBlock value: compiler.
	compiler compile.

	"Then generate the machine code for that method"
	cogInitialAddress := self compile: [
		cogit methodOrBlockNumArgs: argumentCount.
		cogit perform: generatorSelector.
		stopInstruction := cogit Stop
	] bytecodes: 100.
	stopAddress := stopInstruction address
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitive: aSelector with: aBlock [

	| compiler |
	compiler := self compileDruidPrimitiveGenerator: aSelector with: aBlock.

	self
		compileCogitSelector: compiler primitiveCompilerName
		numArgs: compiler numberOfArguments
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> compileDruidPrimitiveGenerator: aSelector with: aBlockCompilerConfig [

	| generatorSelector compiler |
	generatorSelector := (#gen , '_' , aSelector) asSymbol.

	"First generate druid code"
	compiler := compilerCompilerClass new
		            primitiveName: aSelector;
		            interpreter: self interpreter;
		            primitiveCompilerName: generatorSelector;
		            perform: cogitStyle;
		            configureForCompilerClass: compilerClass.

	aBlockCompilerConfig value: compiler.
	compiler compile.
	^ compiler
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
	
	cogit methodOrBlockNumArgs: aCollection size.
	self prepareStackForSendReceiver: receiverOop arguments: aCollection.
	self runFrom: cogInitialAddress until: stopAddress.
]

{ #category : #'helpers-compiling' }
DRPrimitiveCompilationTest >> jitCompilerClass [

	^ compilerClass
]
