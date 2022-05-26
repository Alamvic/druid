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

{ #category : #accessing }
DRBlockClosure >> outerContext [

	^ outerContext
]

{ #category : #accessing }
DRBlockClosure >> outerContext: anObject [

	outerContext := anObject
]
