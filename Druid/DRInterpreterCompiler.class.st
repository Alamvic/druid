Class {
	#name : #DRInterpreterCompiler,
	#superclass : #Object,
	#instVars : [
		'interpreterClass',
		'memory'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #'instance creation' }
DRInterpreterCompiler class >> fromInterpreter: aClass [ 

	^  self new
		interpreterClass: aClass;
		yourself
]

{ #category : #accessing }
DRInterpreterCompiler >> buildInterpreter [

	^  self newInterpreter

]

{ #category : #accessing }
DRInterpreterCompiler >> generateIR [

	| compilationUnit interpreter |
	compilationUnit := DRCompilationUnit new.
	interpreter := self buildInterpreter.
	self interpreterClass primitiveTable do: [ : prim | 
		prim = 0 ifFalse: [ 
		compilationUnit addPrimitive: (DRPrimitiveCompilerCompiler new
			primitiveName: prim ;
			interpreter: interpreter;
			generateDruidIR) ]
		 ].
	^ compilationUnit 
]

{ #category : #accessing }
DRInterpreterCompiler >> initializationOptions [

	^ { 
		  #BytesPerWord.
		  8 .
		  #ObjectMemory.
		  memory class name}
]

{ #category : #accessing }
DRInterpreterCompiler >> interpreterClass [
	^ interpreterClass
]

{ #category : #accessing }
DRInterpreterCompiler >> interpreterClass: aClass [ 

	interpreterClass := aClass
]

{ #category : #accessing }
DRInterpreterCompiler >> newInterpreter [

	memory := self newMemory.

	self interpreterClass
		initializeWithOptions: self initializationOptions
		objectMemoryClass: memory class.

	^ self interpreterClass basicNew
		  objectMemory: memory;
		  basicInitialize
]

{ #category : #accessing }
DRInterpreterCompiler >> newMemory [

	^ Spur64BitCoMemoryManager simulatorClass new
]

{ #category : #accessing }
DRInterpreterCompiler >> primitives [

	^ self generateIR primitives
]
