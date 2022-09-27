Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #inline }
DRMethodIRGenerator >> inlineGenerator [

	^ DRMethodIRGeneratorInline new
		  controlFlowGraph: controlFlowGraph;
		  yourself
]

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments [

	^ self
		  messageSendInstructionFor: aRBMessageNode
		  receiver: receiver
		  arguments: arguments
		  method: (self methodFor: aRBMessageNode withReceiver: receiver)
]

{ #category : #visiting }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstVar
		operands: { aRBVariableNode name asDRValue }
]
