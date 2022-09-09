Class {
	#name : #DRMethodIRGenerator,
	#superclass : #DRIRGenerator,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRMethodIRGenerator >> resolveMessageSend: aRBMessageNode receiver: receiver arguments: arguments method: method [
	self messageSendInstructionFor: aRBMessageNode receiver: receiver arguments: arguments method: method
]
