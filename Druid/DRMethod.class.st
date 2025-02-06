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

{ #category : #activation }
DRMethod >> defaultReturnValue: receiver [

	receiver isInstruction ifTrue: [ ^ receiver ].
	^ DRInterpreterValue value: receiver
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

{ #category : #'accessing - testing' }
DRMethod >> mustAnnotateBytecode [

	| interpreterMethod |
	interpreterMethod := self methodNode compiledMethod.

	"Sends annotate the bytecode by the compiler"
	(interpreterMethod pragmaRefersToLiteral: #hasSend) ifTrue: [ ^ false ].

	"Bytecode needs to be mapped for annotation"
	^ #( isMapped isMappedIfImmutability ) anySatisfy: [ :literal |
		  interpreterMethod pragmaRefersToLiteral: literal ]
]

{ #category : #printing }
DRMethod >> opcode [
	
	^ methodNode selector 
]

{ #category : #accessing }
DRMethod >> operands: aCollection [

	"Do not track users from methods"
	operands := aCollection asOrderedCollection
]
