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

	| irGenerator startBlock initialBasicBlock initial |
	irGenerator := DRPrimitiveIRGenerator new.
	irGenerator instructionFactory: self controlFlowGraph instructionFactory.
	
	"Create the initial basic block"
	startBlock := irGenerator initialBasicBlock.
	initialBasicBlock := irGenerator newBasicBlock.
	startBlock jumpTo: initialBasicBlock.

	irGenerator currentPrimitiveFailureCode: (initial := irGenerator instructionFactory copy: 0).
	initialBasicBlock addInstruction: initial.

	irGenerator numberOfArguments: operands size - 1.
	irGenerator
		interpretCode: (DRMethod methodNode: methodNode)
		receiver: operands first
		arguments: operands allButFirst.
	irGenerator ir.
	1 halt
]

{ #category : #accessing }
DRMessageSend >> isMandatoryInstruction [

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
