Class {
	#name : #DRMessageSend,
	#superclass : #DRInstruction,
	#instVars : [
		'methodNode'
	],
	#category : #'Druid-IR'
}

{ #category : #inlining }
DRMessageSend >> inline [

	| irGenerator pieces |


	pieces := self basicBlock breakBy: self.


	"-.-.-.-.-.-.-.-.-.-.-.-.-."
	irGenerator := DRPrimitiveIRGeneratorInline new.
	irGenerator controlFlowGraph: self controlFlowGraph.

	irGenerator previousBasicBlock: pieces first.
	irGenerator messageSend: self.
	irGenerator nextBasicBlock: pieces last.

	"Create new first block?"
	pieces first jumpTo: irGenerator newBasicBlock.

	irGenerator numberOfArguments: operands size - 1.
	irGenerator
		interpretCode: (DRMethod methodNode: methodNode)
		receiver: operands first
		arguments: operands allButFirst.

	1 halt
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

{ #category : #types }
DRMessageSend >> type [

	1 halt. "TODO"
	^ DRIntegerType new
]
