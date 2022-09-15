Class {
	#name : #DRBlockClosure,
	#superclass : #DRCode,
	#instVars : [
		'outerContext',
		'blockNode'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #activating }
DRBlockClosure >> basicNewFrame [

	^ DRClosureStackFrame new
]

{ #category : #accessing }
DRBlockClosure >> blockNode [

	^ blockNode
]

{ #category : #accessing }
DRBlockClosure >> blockNode: anObject [

	blockNode := anObject
]

{ #category : #accessing }
DRBlockClosure >> codeNode [
	
	^ blockNode
]

{ #category : #executing }
DRBlockClosure >> executeOn: interpreter [
	
	interpreter interpretBlockActivation: blockNode
]

{ #category : #testing }
DRBlockClosure >> isDRBlockClosure [
	
	^ true
]

{ #category : #testing }
DRBlockClosure >> isInstruction [

	^ false
]

{ #category : #asd }
DRBlockClosure >> name [

	^ 'a block'
]

{ #category : #accessing }
DRBlockClosure >> numberOfArguments [
	
	^ blockNode arguments size
]

{ #category : #printing }
DRBlockClosure >> opcode [
	
	^ blockNode asString 
]

{ #category : #accessing }
DRBlockClosure >> outerContext [

	^ outerContext
]

{ #category : #accessing }
DRBlockClosure >> outerContext: anObject [

	outerContext := anObject
]
