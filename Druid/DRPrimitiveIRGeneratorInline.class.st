Class {
	#name : #DRPrimitiveIRGeneratorInline,
	#superclass : #DRPrimitiveIRGenerator,
	#instVars : [
		'previousBasicBlock',
		'messageSend',
		'nextBasicBlock'
	],
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> controlFlowGraph: aDRControlFlowGraph [

	controlFlowGraph := aDRControlFlowGraph
]

{ #category : #asd }
DRPrimitiveIRGeneratorInline >> finishCodeInterpretation: lastFrame [

	"Connect to next basic block"
	currentBasicBlock jumpTo: nextBasicBlock.

	"Remove message send block"
	messageSend replaceUsersBy: lastFrame returnValue.
	nextBasicBlock removePredecessor: messageSend basicBlock.
	messageSend basicBlock removeFromCFG.

]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> messageSend: aDRMessageSend [

	messageSend := aDRMessageSend
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> nextBasicBlock: aDRBasicBlock [

	nextBasicBlock := aDRBasicBlock
]

{ #category : #accessing }
DRPrimitiveIRGeneratorInline >> previousBasicBlock: aDRBasicBlock [

	previousBasicBlock := aDRBasicBlock
]
