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

{ #category : #visiting }
DRCogitPrimitiveCodeGenerator >> visitLoadReceiver: aDRLoadReceiver [

	| sourceRegister destinationRegister |
	sourceRegister := 'ReceiverResultReg'.
	destinationRegister := aDRLoadReceiver result name.

	self copyRegister: sourceRegister to: destinationRegister
]
