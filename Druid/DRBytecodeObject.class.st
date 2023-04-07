Class {
	#name : #DRBytecodeObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'bytecodeNumber',
		'bytecodeSize',
		'startOffset',
		'endOffset'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber [

	^ bytecodeNumber
]

{ #category : #accessing }
DRBytecodeObject >> bytecodeNumber: anObject [

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
DRBytecodeObject >> compileUsing: aCompiler [

	| interpreter |
	interpreter := aCompiler newInterpreter.
	interpreter currentBytecode: bytecodeNumber.
	DRBytecodeCompilerCompiler new
		targetName: self genSelector;
		sourceName: self sourceSelector;
		interpreter: interpreter;
		compilerClass: aCompiler targetClass;
		compile
]

{ #category : #accessing }
DRBytecodeObject >> endOffset [
	" Answer a <Number> representing the end offset when the receiver want to access additional information in the literal frame "
	
	^ endOffset
]

{ #category : #accessing }
DRBytecodeObject >> endOffset: anObject [

	endOffset := anObject
]

{ #category : #accessing }
DRBytecodeObject >> genSelector [
	" Answer a <Symbol> specifying the JITed selector of the interpreter's counterpart (interpreter) method"

	^ #gen_ , self sourceSelector , self bytecodeNumber asString
]

{ #category : #printing }
DRBytecodeObject >> printOn: aStream [

	aStream
		<< 'Bytecode: ';
		<< $#;
		<< self bytecodeNumber asString;
		space;
		<< self sourceSelector
]

{ #category : #accessing }
DRBytecodeObject >> startOffset [
	" Answer a <Number> representing the offset to start when the receiver want to access additional information in the literal frame "
	
	^ startOffset
]

{ #category : #accessing }
DRBytecodeObject >> startOffset: anObject [

	startOffset := anObject
]
