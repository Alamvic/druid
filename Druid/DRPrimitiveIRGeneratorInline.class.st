Class {
	#name : #DRPrimitiveIRGeneratorInline,
	#superclass : #DRPrimitiveIRGeneratorDeferredInline,
	#instVars : [
		'previousBasicBlock',
		'messageSend',
		'endBasicBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> controlFlowGraph: aDRControlFlowGraph [

	controlFlowGraph := aDRControlFlowGraph
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> endBasicBlock: aDRBasicBlock [

	endBasicBlock := aDRBasicBlock
]

{ #category : #visiting }
DRPrimitiveIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	"Connect to next basic block and failure code"
	currentBasicBlock store: executionState primitiveFailureCode in: DRInterpreterReference primitiveFailureCodeAddress.
	currentBasicBlock jumpTo: endBasicBlock.

	"Remove message send block"
	messageSend replaceUsersBy: lastFrame returnValue.
	endBasicBlock removePredecessor: messageSend basicBlock.
	messageSend basicBlock removeFromCFG
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> messageSend: aDRMessageSend [

	messageSend := aDRMessageSend
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> previousBasicBlock: aDRBasicBlock [

	previousBasicBlock := aDRBasicBlock
]
