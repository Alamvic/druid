Class {
	#name : 'DRBytecodeCompilationTestResource',
	#superclass : 'TestResource',
	#instVars : [
		'objectMemory',
		'interpreter'
	],
	#classInstVars : [
		'mode'
	],
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> mode [

	^ mode
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> modeDev [
	"Replace for the bytecode that you are testing"

	<script>
	mode := #bytecodePrimIdenticalSistaV1
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> modeNone [

	<script>
	mode := #none
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> resetMode [

	<script>
	mode := nil
]

{ #category : 'compiling' }
DRBytecodeCompilationTestResource >> compileBytecode: bytecodeAssoc [

	| generatorSelector metacompiler selector bytecodes options |
	options := #( superInstructions ). "TODO: Receive by parameter"

	selector := bytecodeAssoc value.
	bytecodes := { bytecodeAssoc } flattened.

	generatorSelector := ('_' join:
		                      { #gen } , bytecodes asDictionary values)
		                     asSymbol.

	interpreter currentBytecode: bytecodes first key.
	metacompiler := DRBytecodeCompilerCompiler new
		                bytecodes: bytecodes;
		                interpreter: interpreter;
		                targetName: generatorSelector;
		                configureForCompilerClass: self compilerClass;
		                addCompilerOptions: options.

	metacompiler compile
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource >> compilerClass [

	^ DruidTestRTLCompiler
]

{ #category : 'running' }
DRBytecodeCompilationTestResource >> configureInterpreter [
	
	objectMemory := self objectMemoryClass simulatorClass new.
	
	self interpreterClass
		initializeWithOptions: self initializationOptions
		objectMemoryClass: objectMemory class "Spur64BitMMLECoSimulator".

	interpreter := self interpreterClass basicNew
		  objectMemory: objectMemory;
		  basicInitialize.
		
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource >> initializationOptions [

	^ #( #BytesPerWord 8 #ObjectMemory #Spur64BitCoMemoryManager )
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource >> interpreterClass [

	^ DruidTestInterpreter
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource >> objectMemoryClass [

	^ Spur64BitCoMemoryManager
]

{ #category : 'helpers' }
DRBytecodeCompilationTestResource >> setUp [

	self class mode ifNotNil: [ :mode | mode = #none ifTrue: [ ^ self ] ].

	self configureInterpreter.

	'Compiling bytecodes...' traceCr.
	DRBytecodeScenarioCompilationTest bytecodes do: [ :assoc |
		self compileBytecode: assoc ]
]
