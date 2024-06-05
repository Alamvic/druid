Class {
	#name : #DRBytecodeCompilationTest,
	#superclass : #DRDruidTest,
	#category : #'Druid-Tests'
}

{ #category : #enumerating }
DRBytecodeCompilationTest >> compileBytecode: bytecode1 selector: selector1 andBytecode: bytecode2 selector: selector2 thenDo: aFullBlockClosure [ 

		| generatorSelector compiler |
	generatorSelector := ('_' join: { #gen . selector1 . selector2 }) asSymbol.

	self interpreter currentBytecode: bytecode1.
	compiler := DRBytecodeCompilerCompiler new
		bytecodes: {
			bytecode1 -> selector1.
			bytecode2 -> selector2.
		};
		interpreter: self interpreter;
		targetName: generatorSelector;
		configureForCompilerClass: DruidTestRTLCompiler.

	compiler compile.
	"Then generate the machine code for that method"
	cogInitialAddress := self compile: [
		cogit needsFrame: true.
		cogit byte0: bytecode1.
		cogit byte1: bytecode2.
		cogit methodOrBlockNumArgs: 3. "Hack"
		cogit methodOrBlockNumTemps: 2. "Hack"
		"Initialize the simulated stack"
		cogit initSimStackForFramefulMethod: 2.
		cogit zeroOpcodeIndexForNewOpcodes.
		
		aFullBlockClosure value: [ cogit perform: generatorSelector ].
	] bytecodes: 100
]

{ #category : #tests }
DRBytecodeCompilationTest >> compileBytecode: bytecode selector: aSelector thenDo: aBlock [

	| generatorSelector compiler |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.

	"First generate druid code"
	self interpreter currentBytecode: bytecode.
	compiler := DRBytecodeCompilerCompiler new
		bytecodes: { bytecode -> aSelector };
		interpreter: self interpreter;
		configureForCompilerClass: DruidTestRTLCompiler.

	compiler compile.
	"Then generate the machine code for that method"
	cogInitialAddress := self compile: [
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

{ #category : #tests }
DRBytecodeCompilationTest >> doCompileBytecode: bytecode selector: aSelector [

	| compiler |

	"First generate druid code"
	self interpreter currentBytecode: bytecode.
	compiler := DRBytecodeCompilerCompiler new
		sourceName: aSelector;
		interpreter: self interpreter;
		configureForCompilerClass: DruidTestRTLCompiler.

	compiler compile.
]

{ #category : #helpers }
DRBytecodeCompilationTest >> makeFrame: aReceiver arguments: arguments method: aMethod [

	"Callee side - make up the frame"
	"Now push the link register, it will be popped by the return"
	machineSimulator hasLinkRegister ifTrue: [
		self pushAddress: machineSimulator linkRegisterValue ].
	self pushAddress: machineSimulator fp.
	machineSimulator fp: machineSimulator smalltalkStackPointerRegisterValue.
	self pushAddress: aMethod.
	self pushAddress: 0 "context".
	self pushAddress: aReceiver. 

]

{ #category : #helpers }
DRBytecodeCompilationTest >> prepareStackForPrimitiveReceiver: aReceiver arguments: arguments method: aMethod [

	super prepareStackForPrimitiveReceiver: aReceiver arguments: arguments method: aMethod.
	
	self makeFrame: aReceiver arguments: arguments method: aMethod
]

{ #category : #helpers }
DRBytecodeCompilationTest >> prepareStackForSendReceiver: aReceiver arguments: arguments method: aMethod [

	super prepareStackForSendReceiver: aReceiver arguments: arguments method: aMethod.
	
	self makeFrame: aReceiver arguments: arguments method: aMethod.
	
	cogit methodOrBlockNumTemps timesRepeat: [
		self pushAddress: memory nilObject  ]

]
