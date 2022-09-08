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

	| irGenerator pieces firstBasicBlock primitiveFailureCode |


	pieces := self basicBlock breakBy: self.


	"-.-.-.-.-.-.-.-.-.-.-.-.-."
	irGenerator := self newIRGenerator.
	firstBasicBlock := irGenerator newBasicBlock.
	
	irGenerator endBasicBlock: pieces last.
	pieces first jumpTo: firstBasicBlock.
	
	primitiveFailureCode := firstBasicBlock load: DRInterpreterReference primitiveFailureCodeAddress.
	irGenerator currentPrimitiveFailureCode: primitiveFailureCode.
	irGenerator numberOfArguments: operands size - 1.
	irGenerator
		interpretCode: (DRMethod methodNode: methodNode)
		receiver: operands first
		arguments: operands allButFirst.

	self flag: #TODO. "Refactor"
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

{ #category : #generator }
DRMessageSend >> newIRGenerator [

	| irGenerator |
	irGenerator := DRPrimitiveIRGeneratorInline new.
	irGenerator controlFlowGraph: self controlFlowGraph.
	irGenerator messageSend: self.
	^ irGenerator
]

{ #category : #printing }
DRMessageSend >> opcode [
	
	^ 'SEND'
]

{ #category : #types }
DRMessageSend >> type [

	self flag: #TODO. "Unknow type?"
	^ DRIntegerType new
]
