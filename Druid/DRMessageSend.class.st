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

	| pieces firstBasicBlock lastFrame |
	pieces := self basicBlock breakBy: self.
1halt.
	firstBasicBlock := irGenerator firstBasicBlock.
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
	self basicBlock removeFromCFG.

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

	self flag: #TODO. "Unknow type?"
	^ DRIntegerType new
]

{ #category : #copying }
DRMessageSend >> veryDeepCopyWith: deepCopier [
	deepCopier references at: operands put: operands.
	^ super veryDeepCopyWith: deepCopier
]
