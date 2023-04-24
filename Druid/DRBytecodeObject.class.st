Class {
	#name : #DRBytecodeObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'bytecodeSize',
		'bytecodeNumberStart',
		'bytecodeNumberEnd'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #compiling }
DRBytecodeObject >> byteCodeEntries [

	"We need to generate as many entries as there are bytecodes.
	Each entry has a suffix.
	If there is only one entry, do not add a suffix"	
	(supported not or: [bytecodeNumberStart = bytecodeNumberEnd]) ifTrue: [ 
		^ {{
			self bytecodeSize.
			self bytecodeNumberStart.
			self bytecodeNumberEnd.
			self genSelector }} ].
	
	^ (bytecodeNumberStart to: bytecodeNumberEnd) collect: [ :e |
		| suffix |
		suffix := e - bytecodeNumberStart.
		{
			self bytecodeSize.
			e.
			e.
			self genSelector, suffix asString } ]
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber: anInteger [

	bytecodeNumberStart := anInteger.
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

	^ bytecodeNumberStart
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumberStart: anObject [

	bytecodeNumberStart := anObject
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
	self bytecodeNumberStart to: self bytecodeNumberEnd do: [ :e |
		interpreter currentBytecode: e.
		DRBytecodeCompilerCompiler new
			targetName: self genSelector , (e - self bytecodeNumberStart) asString;
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
