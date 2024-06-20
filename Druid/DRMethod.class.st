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
	"Initialize based on pragmas"

	| mappedPragmas |
	interpreter ir hasAnnotatedBytecode ifFalse: [
		(self methodNode compiledMethod pragmaRefersToLiteral: #hasSend)
			ifFalse: [
				mappedPragmas := #( isMapped isMappedIfImmutability ).
				(mappedPragmas anySatisfy: [ :literal |
					 self methodNode compiledMethod pragmaRefersToLiteral: literal ])
					ifTrue: [ interpreter addAnnotateBytecode: self methodNode ] ] ].

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

{ #category : #accessing }
DRMethod >> operands: aCollection [

	"Do not track users from methods"
	operands := aCollection asOrderedCollection
]
