Class {
	#name : 'DRPrimitiveCompilationTestResource',
	#superclass : 'DRBytecodeCompilationTestResource',
	#category : 'Druid-Tests',
	#package : 'Druid-Tests'
}

{ #category : 'accessing' }
DRPrimitiveCompilationTestResource class >> modeDev [
	"Replace for the bytecode that you are testing"

	<script>
	mode := #primitiveAdd.
	self new setUp
]

{ #category : 'helpers' }
DRPrimitiveCompilationTestResource >> compileAll [

	 self compileAllPrimitivesForTesting
]

{ #category : 'compiling' }
DRPrimitiveCompilationTestResource >> compileAllPrimitivesForTesting [

	 | descriptors cogitStyleParameters compilers |
	 descriptors := DRPrimitiveCompilationTest primitives.
	 ('Compiling ' , descriptors size asString , ' primitives for testing...') traceCr.

	 cogitStyleParameters := DRPrimitiveCompilationTest cogitStyleParameter expandMatrix.
	 compilers := ', ' join: (cogitStyleParameters collect: [ :param | param second valuable ]).
	 ('... for ' , compilers , ' compilers...') traceCr.

	descriptors do: [ :desc |
		 cogitStyleParameters do: [ :param |
			 self compilePrimitive: desc with: param ] ]
]

{ #category : 'helpers' }
DRPrimitiveCompilationTestResource >> compileDescriptor: descriptor [

	self compilePrimitive: descriptor with: DRPrimitiveCompilationTest cogitStyleParameter expandMatrix first 
]

{ #category : 'compiling' }
DRPrimitiveCompilationTestResource >> compilePrimitive: primitiveSelector with: param [

	 | generatorSelector compiler cogitStyle compilerClass |
	 generatorSelector := ('gen_' , primitiveSelector) asSymbol.
	 cogitStyle := param first valuable.
	 compilerClass := param second valuable.

	 compiler := DRPrimitiveCompilerCompiler new
		             primitiveName: primitiveSelector;
		             interpreter: interpreter;
		             primitiveCompilerName: generatorSelector;
		             perform: cogitStyle;
		             configureForCompilerClass: compilerClass.

	 ('Compiling: ' , generatorSelector , ' in style: ', cogitStyle) traceCr.

	 compiler compile
]
