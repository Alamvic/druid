Class {
	#name : 'DRBytecodeCompilationTest',
	#superclass : 'DRMetaCompilationTest',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRBytecodeCompilationTest class >> bytecodes [

	^ self subclasses flatCollect: #bytecodes
]

{ #category : 'building suites' }
DRBytecodeCompilationTest class >> cogitStyleParameter [

	^ ParametrizedTestMatrix new
		addCase: { #cogitStyle -> #beForStackToRegisterMapping. #compilerClass -> DruidTestRTLCompiler};
		"addCase: { #cogitStyle -> #beForSimpleStack. #compilerClass -> SimpleDruidTestRTLCompiler};"
		yourself
]

{ #category : 'accessing' }
DRBytecodeCompilationTest class >> resources [

	^Array with: DRBytecodeCompilationTestResource
]

{ #category : 'compiling' }
DRBytecodeCompilationTest >> compileBytecode: aBlock [

	cogInitialAddress := self compile: aBlock bytecodes: 100
]

{ #category : 'enumerating' }
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
		configureForCompilerClass: compilerClass.

	compiler compile.
	"Then generate the machine code for that method"
	cogInitialAddress := self compile: [
		cogit needsFrame: true.
		cogit useTwoPaths: false.
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

{ #category : 'tests' }
DRBytecodeCompilationTest >> compileBytecode: bytecode selector: aSelector options: options thenDo: aBlock [

	| generatorSelector compiler |
	generatorSelector := (#gen, '_' , aSelector) asSymbol.

	"First generate druid code"
	self interpreter currentBytecode: bytecode.
	compiler := DRBytecodeCompilerCompiler new
		            bytecodes: { (bytecode -> aSelector) };
		            interpreter: self interpreter;
						configureForCompilerClass: compilerClass;
		            addCompilerOptions: options.

	compiler compile.
	"Then generate the machine code for that method"
	cogInitialAddress := self
		                     compile: [
			                     cogit needsFrame: true.
			                     cogit useTwoPaths: false.
			                     cogit byte0: bytecode.
			                     cogit methodOrBlockNumArgs: 3. "Hack"
			                     cogit methodOrBlockNumTemps: 2. "Hack"
			                     "Initialize the simulated stack"
			                     cogit initSimStackForFramefulMethod: 2.
			                     cogit zeroOpcodeIndexForNewOpcodes.

			                     aBlock value: [
				                     cogit perform: generatorSelector ] ]
		                     bytecodes: 100
]

{ #category : 'tests' }
DRBytecodeCompilationTest >> compileBytecode: bytecode selector: aSelector thenDo: aBlock [

	^ self
		  compileBytecode: bytecode
		  selector: aSelector
		  options: #(  )
		  thenDo: aBlock
]

{ #category : 'compiling' }
DRBytecodeCompilationTest >> compileBytecodeSelector: aSelector [

	 self compileBytecodeSelector: aSelector sufix: ''
]

{ #category : 'compiling' }
DRBytecodeCompilationTest >> compileBytecodeSelector: selector1 andSelector: selector2 [

	| generatorSelector bytecodes |
	generatorSelector := (#gen , '_' , selector1 , '_' , selector2) asSymbol.
	bytecodes := self class bytecodes detect: [ :descriptor |
			             descriptor first value = selector1 and: [
				             descriptor second value = selector2 ] ].

	cogit byte0: bytecodes first key.
	cogit byte1: bytecodes second key.
	cogit perform: generatorSelector
]

{ #category : 'compiling' }
DRBytecodeCompilationTest >> compileBytecodeSelector: aSelector sufix: sufix [

	 | generatorSelector currentBytecode |
	 generatorSelector := (#gen , '_' , aSelector, sufix) asSymbol.
	 currentBytecode := self class bytecodes
		                    detect: [ :array | array first value = aSelector ]
		                    ifFound: [ :array | array first key ]
		                    ifNone: [ self error: 'Missing bytecode descriptor...' ].
	 cogit byte0: currentBytecode.
	 cogit perform: generatorSelector
]

{ #category : 'compiling' }
DRBytecodeCompilationTest >> compileBytecodeSelector: aSelector version: version [

	 self
		 compileBytecodeSelector: aSelector
		 sufix: '_version' , version asString
]

{ #category : 'tests' }
DRBytecodeCompilationTest >> doCompileBytecode: bytecode selector: aSelector [

	| compiler |

	"First generate druid code"
	self interpreter currentBytecode: bytecode.
	compiler := DRBytecodeCompilerCompiler new
		sourceName: aSelector;
		interpreter: self interpreter;
		configureForCompilerClass: compilerClass.

	compiler compile.
]

{ #category : 'initialization' }
DRBytecodeCompilationTest >> initializeSStack: argCount [

	self isSimpleStack ifTrue: [ ^ self ]. "No SSTack"

	cogit ssPushRegister: ReceiverResultReg.
	argCount > 0 ifTrue: [ cogit ssPushRegister: Arg0Reg ].
	argCount > 1 ifTrue: [ cogit ssPushRegister: Arg1Reg ]
]

{ #category : 'helpers' }
DRBytecodeCompilationTest >> makeFrame: aReceiver arguments: arguments method: aMethod [
	"Callee side - make up the frame"

	"Now push the link register, it will be popped by the return"
	machineSimulator hasLinkRegister ifTrue: [
		self pushAddress: machineSimulator linkRegisterValue ].
	self pushAddress: machineSimulator fp.
	machineSimulator fp:
		machineSimulator smalltalkStackPointerRegisterValue.
	self pushAddress: aMethod.
	self pushAddress: 0. "context"
	self pushAddress: aReceiver.
	"arguments do: [ :anArgument | self pushAddress: anArgument ]."

]

{ #category : 'as yet unclassified' }
DRBytecodeCompilationTest >> popAndReturn: reg [

	self isSimpleStack
		ifTrue: [ cogit PopR: reg ]
		ifFalse: [ cogit ssPopTopToReg: reg ].
	cogit genUpArrowReturn
]

{ #category : 'helpers' }
DRBytecodeCompilationTest >> prepareStackForPrimitiveReceiver: aReceiver arguments: arguments method: aMethod [

	super prepareStackForPrimitiveReceiver: aReceiver arguments: arguments method: aMethod.
	
	self makeFrame: aReceiver arguments: arguments method: aMethod
]

{ #category : 'helpers' }
DRBytecodeCompilationTest >> prepareStackForSendReceiver: aReceiver arguments: arguments method: aMethod [

	super prepareStackForSendReceiver: aReceiver arguments: arguments method: aMethod.

	self makeFrame: aReceiver arguments: arguments method: aMethod.

	self flag: #TODO. "Receive this by param, method is not used for the tests"
	cogit methodOrBlockNumTemps timesRepeat: [
		self pushAddress: memory nilObject  ]

]

{ #category : 'helpers' }
DRBytecodeCompilationTest >> setUp [

	super setUp.
	cogit needsFrame: true.
	cogit useTwoPaths: false.
	cogit methodOrBlockNumArgs: 3. "Hack"
	cogit methodOrBlockNumTemps: 2. "Hack"
	"Initialize the simulated stack"
	cogit initSimStackForFramefulMethod: 2.
	cogit zeroOpcodeIndexForNewOpcodes
]
