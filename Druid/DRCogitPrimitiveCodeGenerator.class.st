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
DRCogitPrimitiveCodeGenerator >> returnNodeForCompiledMethod: aDRControlFlowGraph [

	| primitiveFail isCompletePrimitive |
	primitiveFail := aDRControlFlowGraph failureExitBlock instructions
		                 unique.
	isCompletePrimitive := primitiveFail operand1 value.

	isCompletePrimitive ifTrue: [
		^ RBVariableNode named: 'CompletePrimitive' ].

	^ super returnNodeForCompiledMethod: aDRControlFlowGraph
]

{ #category : #visiting }
DRCogitPrimitiveCodeGenerator >> visitLoadReceiver: aDRLoadReceiver [

	| sourceRegister destinationRegister |
	sourceRegister := 'ReceiverResultReg'.
	destinationRegister := aDRLoadReceiver result name.

	self copyRegister: sourceRegister to: destinationRegister
]
