Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitLoadReceiver: aDRLoadReceiver [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ensureReceiverResultRegContainsSelf).

	self
		twoOperandCogitRTL: #Move
		operands: { 
				(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg').
				aDRLoadReceiver result }
		instruction: aDRLoadReceiver
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitPush: aDRPush [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: 'ssPush',
				 (aDRPush operand1 rtlPushOperandQualifierForInstruction: aDRPush)
			 arguments: aDRPush operand1 rtlPushArgumentExpressions)
]
