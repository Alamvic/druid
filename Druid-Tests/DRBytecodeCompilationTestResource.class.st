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
DRBytecodeCompilationTestResource class >> compileAll [

	 <script>
	 self resetMode.
	 self new setUp
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> mode [

	^ mode
]

{ #category : 'accessing' }
DRBytecodeCompilationTestResource class >> modeDev [
	"Replace for the bytecode that you are testing"

	<script>
	mode := { (83 -> #duplicateTopBytecode) }
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
DRBytecodeCompilationTestResource >> compileAllBytecodeForTesting [

	 | descriptors |
	 descriptors := DRBytecodeCompilationTest bytecodes.
	 ('Compiling ' , descriptors size asString , ' bytecodes for testing...') traceCr.
	 descriptors do: [ :desc | self compileBytecode: desc ]
]

{ #category : 'compiling' }
DRBytecodeCompilationTestResource >> compileBytecode: descriptor [

	 | generatorSelector metacompiler options bytecodes |
	 bytecodes := descriptor.
	 options := #( superInstructions ).

	 descriptor last isArray ifTrue: [
		 options := options , descriptor last.
		 bytecodes := descriptor allButLast ].

	 generatorSelector := ('_' join:
		                       { #gen } , bytecodes asDictionary values)
		                      asSymbol.

	 options
		 detect: [ :opt | opt beginsWith: 'version' ]
		 ifFound: [ :suffix | generatorSelector := generatorSelector , '_' , suffix ].

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

	 self class mode ifNotNil: [ :mode | "Avoid compilation if mode is none"
		 mode = #none ifTrue: [ ^ self ] ].

	 "Compile..."
	 self configureInterpreter.
	 self class mode
		 ifNil: [ self compileAllBytecodeForTesting ]
		 ifNotNil: [ :descriptor | self compileBytecode: descriptor ].

	 "On CI, we want to execute this setUp only once"
	 self onPharoCITestingEnvironment ifTrue: [ self class modeNone ]
]
