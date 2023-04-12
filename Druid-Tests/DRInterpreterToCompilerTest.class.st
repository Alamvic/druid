Class {
	#name : #DRInterpreterToCompilerTest,
	#superclass : #DRAbstractCompilerCollectorTest,
	#instVars : [
		'interpreterToCompiler'
	],
	#category : #'Druid-Tests-CompilerBuilder'
}

{ #category : #running }
DRInterpreterToCompilerTest >> setUp [

	super setUp.
	interpreterToCompiler := (DRInterpreterToCompiler fromInterpreterClass:
			                          DRBasicCogInterpreterArithmeticPrimitives)
		                         doFailOnFirst;
		                         targetClass: self jitCompilerClassForTest;
		                         yourself
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testEmptyInterpreterGeneratesEmptyCompilationUnits [

	interpreterToCompiler
		interpreterClass: DREmptyInterpreter;
		build.
	self assert: interpreterToCompiler primitives isEmpty.
	self assert: interpreterToCompiler bytecodes isEmpty
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testErrorHandling [

	interpreterToCompiler := DRInterpreterToCompiler fromInterpreterClass: DruidTestInterpreter.
	interpreterToCompiler 
		targetClass: self jitCompilerClassForTest;
		selectPrimitives: [ : p | p selector = #primitiveDruidFail ];
		buildAndCompileIn: self jitCompilerClassNameForTest.

	self assert: interpreterToCompiler errors notEmpty.
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGeneratesBytecodeCompilationUnits [

	| currentBytecodeNumber |
	interpreterToCompiler build.

	self assert: interpreterToCompiler bytecodes isNotEmpty.

	"Start on bytecode 0"
	currentBytecodeNumber := 0.
	interpreterToCompiler bytecodes doWithIndex: [ :bytecodeUnit :index |
		self assert: bytecodeUnit bytecodeSize equals: index.
		self assert: bytecodeUnit bytecodeNumberStart >= currentBytecodeNumber.
		currentBytecodeNumber := bytecodeUnit bytecodeNumberStart.
		self assert: bytecodeUnit bytecodeNumberEnd >= currentBytecodeNumber.
		currentBytecodeNumber := bytecodeUnit bytecodeNumberEnd.
		self assert: bytecodeUnit genSelector equals: #unknownBytecode ].

	"Finish on bytecode 255"
	self assert: currentBytecodeNumber equals: 255
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testGeneratesPrimitiveCompilationUnits [

	| primitiveSet |
	primitiveSet := interpreterToCompiler interpreterClass primitiveTable reject: [
		                :prim | prim isNumber ].

	interpreterToCompiler build.

	self
		assertCollection: (interpreterToCompiler primitives collect: #sourceSelector)
		hasSameElements: primitiveSet
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testMandatoryPrimitiveCompilationUnits [

	| supportedPrimitive mandatoryPrimitives |
	interpreterToCompiler
		interpreterClass: DruidTestInterpreter;
		selectPrimitives: [ :p | p selector = #primitiveAdd ];
		build.

	supportedPrimitive := interpreterToCompiler primitives first.
	self assert: supportedPrimitive sourceSelector equals: #primitiveAdd.
	self assert: supportedPrimitive genSelector equals: #gen_primitiveAdd.
	self assert: supportedPrimitive supported.
	self deny: supportedPrimitive mayCallback.

	mandatoryPrimitives := interpreterToCompiler primitives allButFirst.
	self assert: mandatoryPrimitives isNotEmpty.
	mandatoryPrimitives do: [ :mandatoryPrimitive |
		self
			assert: mandatoryPrimitive genSelector
			equals: #genNonImplementedPrimitive.
		self deny: mandatoryPrimitive supported.
		self assert: mandatoryPrimitive mayCallback ]
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testSelectBytecodeCompilationUnits [

	| supportedBytecode unknownBytecodes currentBytecodeNumber |
	interpreterToCompiler
		selectBytecodes: [ :cm | cm selector = #pushReceiverVariableBytecode ];
		build.

	supportedBytecode := interpreterToCompiler bytecodes first.
	self
		assert: supportedBytecode bytecodeSize
		equals: 1.
	self
		assert: supportedBytecode bytecodeNumberStart
		equals: 0.
	self
		assert: supportedBytecode bytecodeNumberEnd
		equals: 15.
	self
		assert: supportedBytecode sourceSelector
		equals: #pushReceiverVariableBytecode.
	self
		assert: supportedBytecode genSelector
		equals: #gen_pushReceiverVariableBytecode.
	self assert: supportedBytecode supported.

	unknownBytecodes := interpreterToCompiler bytecodes allButFirst.
	self assert: unknownBytecodes isNotEmpty.
	
	"Continue from previous bytecode"
	currentBytecodeNumber := supportedBytecode bytecodeNumberEnd.
	unknownBytecodes doWithIndex: [ :bytecodeUnit :index |
		self assert: bytecodeUnit bytecodeSize equals: index.
		self assert: bytecodeUnit bytecodeNumberStart > currentBytecodeNumber.
		currentBytecodeNumber := bytecodeUnit bytecodeNumberStart.
		self assert: bytecodeUnit bytecodeNumberEnd > currentBytecodeNumber.
		currentBytecodeNumber := bytecodeUnit bytecodeNumberEnd.
		self assert: bytecodeUnit genSelector equals: #unknownBytecode ].

	"Finish on bytecode 255"
	self assert: currentBytecodeNumber equals: 255
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testSelectPrimitiveCompilationUnits [

	interpreterToCompiler
		selectPrimitives: [ :p | p selector = #primitiveAdd ];
		build.

	self
		assertCollection:
		(interpreterToCompiler primitives collect: [ :p | p sourceSelector ])
		hasSameElements: { #primitiveAdd }
]

{ #category : #tests }
DRInterpreterToCompilerTest >> testSelectSingleBytecodeNumberCompilationUnits [

	| supportedBytecode |
	interpreterToCompiler
		selectBytecodes: [ :cm | cm selector = #pushConstantTrueBytecode ];
		build.

	supportedBytecode := interpreterToCompiler bytecodes first.
	self
		assert: supportedBytecode bytecodeSize
		equals: 1.
	self
		assert: supportedBytecode bytecodeNumberStart
		equals: 77.
	self
		assert: supportedBytecode bytecodeNumberEnd
		equals: 77.
	self
		assert: supportedBytecode sourceSelector
		equals: #pushConstantTrueBytecode.
	self
		assert: supportedBytecode genSelector
		equals: #gen_pushConstantTrueBytecode.
	self assert: supportedBytecode supported.

]
