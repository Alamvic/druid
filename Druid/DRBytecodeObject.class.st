Class {
	#name : #DRBytecodeObject,
	#superclass : #DRInterpreterInstruction,
	#instVars : [
		'bytecodeSize',
		'bytecodeNumberEnd',
		'bytecodeNumber',
		'targetSelector'
	],
	#category : #'Druid-CompilerBuilder'
}

{ #category : #compiling }
DRBytecodeObject >> annotatedDescriptors [

	| complementaryThings |
	supported ifFalse: [ ^ {  } ].

	complementaryThings := OrderedCollection new.
	sourceMethod pragmasDo: [ :pragma |
		pragma selector = #compilationInfo: ifTrue: [
			complementaryThings add: pragma arguments unique ] ].
	"Last"
	sourceMethod pragmas
		detect: [ :pragma | pragma selector beginsWith: #needsFrame ]
		ifFound: [ :pragma |
			complementaryThings add: pragma selector.
			complementaryThings add: pragma arguments unique ].
	^ complementaryThings
]

{ #category : #compiling }
DRBytecodeObject >> byteCodeEntries [

	^ (bytecodeNumber to: bytecodeNumberEnd) collect: [ :e |
		  | suffix |
		  suffix := e - bytecodeNumber.
		  {
			  self bytecodeSize.
			  e.
			  e.
			  self genSelector }, self annotatedDescriptors ]
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
	self bytecodeNumberStart to: self bytecodeNumberEnd do: [ :e |
		interpreter currentBytecode: e.
		DRBytecodeCompilerCompiler new
			targetName: self genSelector;
			sourceName: self sourceSelector;
			interpreter: interpreter;
			configureForCompilerClass: aCompiler targetClass;
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

{ #category : #accessing }
DRBytecodeObject >> targetSelector [

	^ targetSelector ifNil: [ super targetSelector ]
]

{ #category : #accessing }
DRBytecodeObject >> targetSelector: anObject [

	targetSelector := anObject
]
