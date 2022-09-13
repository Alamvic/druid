Class {
	#name : #DRMessageSend,
	#superclass : #DRInstruction,
	#instVars : [
		'methodNode',
		'irGenerator'
	],
	#category : #'Druid-IR'
}

{ #category : #inlining }
DRMessageSend >> inline [

	| pieces firstBasicBlock lastFrame inlinedMethodClass |
	methodNode ifNil: [ 
		inlinedMethodClass := self receiverType.
		inlinedMethodClass ifNil: [ ^ self ].
		self methodNode:
			(inlinedMethodClass lookupSelector: self originAST selector) ast ].

	pieces := self basicBlock breakBy: self.

	firstBasicBlock := irGenerator newFirstBasicBlock.
	pieces first jumpTo: firstBasicBlock.

	irGenerator numberOfArguments: operands size - 1.
	lastFrame := irGenerator
		             interpretCode: (DRMethod methodNode: methodNode)
		             receiver: operands first
		             arguments: operands allButFirst.

	irGenerator currentBasicBlock jumpTo: pieces last.
	"Remove message send block"
	self replaceUsersBy: lastFrame returnValue.
	pieces last removePredecessor: self basicBlock.
	self basicBlock removeFromCFG
]

{ #category : #accessing }
DRMessageSend >> irGenerator: anIrGenerator [ 

	irGenerator := anIrGenerator
]

{ #category : #accessing }
DRMessageSend >> isMandatoryInstruction [

	^ true
]

{ #category : #accessing }
DRMessageSend >> isMessageSend [

	^ true
]

{ #category : #accessing }
DRMessageSend >> methodNode [

	^ methodNode
]

{ #category : #accessing }
DRMessageSend >> methodNode: anObject [

	methodNode := anObject
]

{ #category : #printing }
DRMessageSend >> opcode [
	
	^ 'SEND'
]

{ #category : #inlining }
DRMessageSend >> receiverType [

	| receiverClass typePragmas receiverName receiverTypePragma |
	typePragmas := self originAST methodNode allPragmasNamed: #var:type:.
	receiverName := self originAST receiver name.
	receiverTypePragma := typePragmas detect: [ :pragma | 
		                      pragma arguments first value = receiverName ]
		                      ifNone: [ ^ nil ].
	receiverClass := self class environment at: (receiverTypePragma arguments last value).
	^ receiverClass
]

{ #category : #SCCP }
DRMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"

	
]

{ #category : #types }
DRMessageSend >> type [

	self flag: #TODO. "Unknow type?"
	^ DRIntegerType new
]

{ #category : #copying }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
