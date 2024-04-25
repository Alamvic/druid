Class {
	#name : #DRCogitPrimitiveCodeGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitPrimitiveCodeGenerator >> copyRegister: sourceRegister to: destinationRegister [
	
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #MoveR:R:
			 arguments: {
					 (RBVariableNode named: sourceRegister).
					 (RBVariableNode named: destinationRegister) })
]

{ #category : #'ir-to-ast' }
DRCogitPrimitiveCodeGenerator >> linearizeBlocksInCFG: aControlFlowGraph [

	| linearizedBlocks |
	linearizedBlocks := super linearizeBlocksInCFG: aControlFlowGraph.

	"Make sure the failure exit block is at the end, so it works as a fallthrough with the following bytecode"
	linearizedBlocks removeAll: aControlFlowGraph failureExitBlocks.
	linearizedBlocks addAllLast: aControlFlowGraph failureExitBlocks.

	^ linearizedBlocks
]

{ #category : #'ir-to-ast' }
DRCogitPrimitiveCodeGenerator >> returnNodeForCompiledMethod: aDRControlFlowGraph [

	| isCompletePrimitive |
	isCompletePrimitive := aDRControlFlowGraph failureExitBlocks
		                       allSatisfy: [ :b |
			                       | primitiveFail |
			                       primitiveFail := b endInstruction.
			                       primitiveFail operand1 value ].

	isCompletePrimitive ifTrue: [
		^ RBVariableNode named: 'CompletePrimitive' ].

	^ super returnNodeForCompiledMethod: aDRControlFlowGraph
]

{ #category : #visiting }
DRCogitPrimitiveCodeGenerator >> visitFlushStack: aDRFlushStack [ 

	"Nothing! There are not sym stack on primitives."
	"TODO: Avoid DRFlushStack instructions on primitive meta-compilation"
]

{ #category : #visiting }
DRCogitPrimitiveCodeGenerator >> visitLoadReceiver: aDRLoadReceiver [

	| sourceRegister destinationRegister |
	sourceRegister := 'ReceiverResultReg'.
	destinationRegister := aDRLoadReceiver result name.

	self copyRegister: sourceRegister to: destinationRegister
]
