Class {
	#name : #DRBytecodeObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'bytecodeSize',
		'bytecodeNumberEnd',
		'bytecodeNumber'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #compiling }
DRBytecodeObject >> byteCodeEntries [

	"We need to generate as many entries as there are bytecodes.
	Each entry has a suffix.
	If there is only one entry, do not add a suffix"	
	(supported not or: [bytecodeNumber = bytecodeNumberEnd]) ifTrue: [ 
		^ {{
			self bytecodeSize.
			self bytecodeNumberStart.
			self bytecodeNumberEnd.
			self genSelector }} ].
	
	^ (bytecodeNumber to: bytecodeNumberEnd) collect: [ :e |
		| suffix |
		suffix := e - bytecodeNumber.
		{
			self bytecodeSize.
			e.
			e.
			self genSelector, suffix asString } ]
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber: anInteger [

	bytecodeNumber := anInteger.
	bytecodeNumberEnd := anInteger
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumberEnd [

	^ bytecodeNumberEnd
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumberEnd: anObject [

	bytecodeNumberEnd := anObject
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumberStart [

	^ bytecodeNumber
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumberStart: anObject [

	bytecodeNumber := anObject
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeSize [

	^ bytecodeSize
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeSize: anObject [

	bytecodeSize := anObject
]

{ #category : #compiling }
DRBytecodeObject >> compileUnitUsing: aCompiler [

	| interpreter |
	interpreter := aCompiler newInterpreter.
	"If this is a single bytecode range, compile the method without suffix"
	self bytecodeNumberStart = self bytecodeNumberEnd ifTrue: [
		interpreter currentBytecode: self bytecodeNumberStart.
		^ DRBytecodeCompilerCompiler new
			targetName: self genSelector;
			sourceName: self sourceSelector;
			interpreter: interpreter;
			compilerClass: aCompiler targetClass;
			compile ].
	
	"Otherwise iterate the bytecode and compile a variant per suffix"
	self bytecodeNumberStart to: self bytecodeNumberEnd do: [ :e |
		| suffix |
		suffix := e - bytecodeNumber.
		interpreter currentBytecode: e.
		DRBytecodeCompilerCompiler new
			targetName: self genSelector , suffix asString;
			sourceName: self sourceSelector;
			interpreter: interpreter;
			compilerClass: aCompiler targetClass;
			compile ]
]

{ #category : #accessing }
DRBytecodeObject >> notSupportedSelector [

	^ #unknownBytecode
]

{ #category : #printing }
DRBytecodeObject >> printOn: aStream [

	aStream
		<< $(
		<< self bytecodeSize asString;
		<< $);
		space;
		<< self bytecodeNumberStart asString;
		<< ' - ';
		<< self bytecodeNumberEnd asString;
		space;
		<< self genSelector
]
