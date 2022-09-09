Class {
	#name : #DRPrimitiveIRGeneratorInline,
	#superclass : #DRPrimitiveIRGenerator,
	#instVars : [
		'previousBasicBlock',
		'messageSend',
		'endBasicBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #visiting }
DRPrimitiveIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	"Connect to next basic block and failure code"
	currentBasicBlock store: executionState primitiveFailureCode in: DRInterpreterReference primitiveFailureCodeAddress.

]

{ #category : #initialization }
DRPrimitiveIRGeneratorInline >> initialize [
	super initialize.
	self currentPrimitiveFailureCode: (self newBasicBlock load:
			 DRInterpreterReference primitiveFailureCodeAddress)
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> messageSend: aDRMessageSend [

	messageSend := aDRMessageSend
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> previousBasicBlock: aDRBasicBlock [

	previousBasicBlock := aDRBasicBlock
]
