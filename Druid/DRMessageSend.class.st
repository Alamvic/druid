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

	| irGenerator pieces firstBasicBlock |
	irGenerator := self newIRGenerator.
	pieces := self basicBlock breakBy: self.

	firstBasicBlock := irGenerator newBasicBlock.
	pieces first jumpTo: firstBasicBlock.
	irGenerator endBasicBlock: pieces last.

	irGenerator currentPrimitiveFailureCode: (firstBasicBlock load:
			 DRInterpreterReference primitiveFailureCodeAddress).
	irGenerator numberOfArguments: operands size - 1.
	irGenerator
		interpretCode: (DRMethod methodNode: methodNode)
		receiver: operands first
		arguments: operands allButFirst.

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

{ #category : #SCCP }
DRMessageSend >> sccpEvaluateFor: sccp [

	"Nothing to do"

	
]

{ #category : #types }
DRMessageSend >> type [

	(methodNode hasPragmaNamed: #returnTypeC:) ifTrue: [ "TODO: Match Slang types" 
		1 halt ].

	"Default type"
	^ DRIntegerType new
]

{ #category : #copying }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
