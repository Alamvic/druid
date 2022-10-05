Class {
	#name : #DRBytecodeCompilationTest,
	#superclass : #DRDruidTests,
	#category : #'Druid-Tests'
}

{ #category : #tests }
DRBytecodeCompilationTest >> compileBytecode: bytecode selector: aSelector thenDo: aBlock [

	| generatorSelector compiler |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.

	"First generate druid code"
	self interpreter currentBytecode: bytecode.
	compiler := DRBytecodeCompilerCompiler new
		sourceName: aSelector;
		interpreter: self interpreter;
		compilerClass: DruidTestRTLCompiler.
	compiler compile.

	"Then generate the machine code for that method"
	initialAddress := self compile: [
		cogit needsFrame: true.
		cogit byte0: bytecode.
		cogit methodOrBlockNumArgs: 3. "Hack"
		cogit methodOrBlockNumTemps: 2. "Hack"
		"Initialize the simulated stack"
		cogit initSimStackForFramefulMethod: 2.
		cogit zeroOpcodeIndexForNewOpcodes.
		
		aBlock value: [ cogit perform: generatorSelector ].
	] bytecodes: 100
]

{ #category : #helpers }
DRBytecodeCompilationTest >> prepareStackForSendReceiver: aReceiver arguments: arguments [

	super prepareStackForSendReceiver: aReceiver arguments: arguments.

	"Callee side - make up the frame"
	"Now push the link register, it will be popped by the return"
	machineSimulator hasLinkRegister ifTrue: [
		self pushAddress: machineSimulator linkRegisterValue ].
	self pushAddress: machineSimulator fp.
	machineSimulator fp: machineSimulator smalltalkStackPointerRegisterValue.
	self pushAddress: 0 "Method".
	self pushAddress: 0 "context".
	self pushAddress: aReceiver
]
