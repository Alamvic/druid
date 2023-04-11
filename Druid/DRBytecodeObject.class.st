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
	interpreter currentBytecode: self bytecodeNumberStart.
	DRBytecodeCompilerCompiler new
		targetName: self genSelector;
		sourceName: self sourceSelector;
		interpreter: interpreter;
		compilerClass: aCompiler targetClass;
		compile
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
