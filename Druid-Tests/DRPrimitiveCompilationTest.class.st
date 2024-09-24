Class {
	#name : #DRPrimitiveCompilationTest,
	#superclass : #DRMetaCompilationTest,
	#instVars : [
		'stopAddress'
	],
	#category : #'Druid-Tests'
}

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
DRPrimitiveCompilationTest >> compileDruidPrimitiveGenerator: primitiveSelector with: aBlock [

	| compiler |
	"First generate druid code"
	compiler := DRPrimitiveCompilerCompiler new
		            primitiveName: primitiveSelector;
		            interpreter: self interpreter;
		            primitiveCompilerName: (self generatorSelector: primitiveSelector);
		            perform: cogitStyle;
		            configureForCompilerClass: compilerClass.

	aBlock value: compiler.
	compiler compile.
	^ compiler
]

{ #category : #compiling }
DRPrimitiveCompilationTest >> compilePrimitive: primitiveSelector ForSendReceiver: receiver arguments: arguments [

	self
		compileDruidPrimitiveGenerator: primitiveSelector
		with: [ :c | "nothing" ].

	self
		compileCogitSelector: (self generatorSelector: primitiveSelector)
		numArgs: arguments size.

	self prepareStackForSendReceiver: receiver arguments: arguments
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

{ #category : #'helpers - execution' }
DRPrimitiveCompilationTest >> runUntilStop [

	self runFrom: cogInitialAddress until: stopAddress
]
