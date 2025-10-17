Class {
	#name : 'DRPrimitiveIRGeneratorInline',
	#superclass : 'DRPrimitiveIRGeneratorDeferredInline',
	#instVars : [
		'previousBasicBlock',
		'messageSend',
		'endBasicBlock'
	],
	#category : 'Druid-Cogit-Metacompiler',
	#package : 'Druid-Cogit',
	#tag : 'Metacompiler'
}

{ #category : 'visiting' }
DRPrimitiveIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	"Connect to next basic block and failure code"
	currentBasicBlock storeSInt64: executionState primitiveFailureCode at: DRInterpreterReference primitiveFailureCodeAddress.

]

{ #category : 'accessing' }
DRPrimitiveIRGeneratorInline >> messageSend: aDRMessageSend [

	messageSend := aDRMessageSend
]

{ #category : 'building' }
DRPrimitiveIRGeneratorInline >> newFirstBasicBlock [

	| block |
	block := self newBasicBlock.
	self currentPrimitiveFailureCode:
		(block load: DRInterpreterReference primitiveFailureCodeAddress).
	^ block
]

{ #category : 'accessing' }
DRPrimitiveIRGeneratorInline >> previousBasicBlock: aDRBasicBlock [

	previousBasicBlock := aDRBasicBlock
]
