"
Abstract class to provide a source method, i.e., for example, in the interpreter the source method contains the method which the primitive implements.

"
Class {
	#name : 'DRInterpreterInstruction',
	#superclass : 'Object',
	#instVars : [
		'sourceMethod',
		'supported'
	],
	#category : 'Druid-CompilerBuilder',
	#package : 'Druid',
	#tag : 'CompilerBuilder'
}

{ #category : 'compiling' }
DRInterpreterInstruction >> compileUnitUsing: aCompiler [

	self subclassResponsibility
]

{ #category : 'compiling' }
DRInterpreterInstruction >> compileUsing: aCompiler [

	self supported ifFalse: [ ^ self ].

	self compileUnitUsing: aCompiler
]

{ #category : 'accessing' }
DRInterpreterInstruction >> genSelector [
	" Answer a <Symbol> specifying the JITed selector of the interpreter's counterpart (compiler) method"

	supported ifFalse: [ ^ self notSupportedSelector ].

	^ self targetSelector
]

{ #category : 'testing' }
DRInterpreterInstruction >> hasPragmaNamed: aString [

	^ self sourceMethod hasPragmaNamed: aString
]

{ #category : 'initialization' }
DRInterpreterInstruction >> initialize [

	supported := true
]

{ #category : 'accessing' }
DRInterpreterInstruction >> notSupportedSelector [
	"Should return the selector used when this instruction is not supported in the compiler"

	self subclassResponsibility
]

{ #category : 'accessing' }
DRInterpreterInstruction >> sourceMethod [

	^ sourceMethod
]

{ #category : 'accessing' }
DRInterpreterInstruction >> sourceMethod: anObject [

	sourceMethod := anObject
]

{ #category : 'accessing' }
DRInterpreterInstruction >> sourceSelector [

	^ self sourceMethod
		  ifNil: [ self notSupportedSelector ]
		  ifNotNil: [ :m | m selector ]
]

{ #category : 'accessing' }
DRInterpreterInstruction >> supported [

	^ supported
]

{ #category : 'accessing' }
DRInterpreterInstruction >> supported: aBoolean [

	supported := aBoolean
]

{ #category : 'accessing' }
DRInterpreterInstruction >> targetSelector [
	"Answer a <Symbol> specifying the JITed selector of the interpreter's counterpart (compiler) method"

	^ #gen_ , self sourceSelector capitalized
]
