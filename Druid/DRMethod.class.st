Class {
	#name : #DRMethod,
	#superclass : #DRCode,
	#instVars : [
		'methodNode'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRMethod class >> methodNode: aMethodNode [

	^ self new
		methodNode: aMethodNode;
		yourself
]

{ #category : #accessing }
DRMethod >> basicNewFrame [

	^ DRStackFrame new
]

{ #category : #accessing }
DRMethod >> codeNode [
	
	^ self methodNode
]

{ #category : #executing }
DRMethod >> executeOn: interpreter [ 

	self codeNode acceptVisitor: interpreter
]

{ #category : #accessing }
DRMethod >> methodNode [

	^ methodNode
]

{ #category : #accessing }
DRMethod >> methodNode: anObject [

	methodNode := anObject
]

{ #category : #printing }
DRMethod >> opcode [
	
	^ methodNode selector 
]
