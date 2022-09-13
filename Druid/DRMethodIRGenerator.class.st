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
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments method: method [
	self messageSendInstructionFor: aRBMessageNode receiver: receiver arguments: arguments method: method
]

{ #category : #visiting }
DRMethodIRGenerator >> visitInstanceVariableNode: aRBVariableNode [

	self
		addInstructionFrom: aRBVariableNode
		instructionKind: DRLoadInstVar
		operands: { aRBVariableNode name asDRValue }
]
