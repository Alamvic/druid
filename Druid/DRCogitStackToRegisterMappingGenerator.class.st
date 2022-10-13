Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	self
		twoOperandCogitRTL: #Move
		operands: { 
				(DRPhysicalGeneralPurposeRegister name: 'FPReg').
				aDRLoadFramePointer result }
		instruction: aDRLoadFramePointer
]

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
DRCogitStackToRegisterMappingGenerator >> visitLoadStackValue: aDRLoadStackValue [

	"(self ssDescriptorAt: 0) moveToReg: TempReg."

	| sourceDescriptorAST |
	sourceDescriptorAST := RBMessageNode
		                       receiver: RBVariableNode selfNode
		                       selector: #ssDescriptorAt:
		                       arguments:
		                       aDRLoadStackValue operand1
			                       rtlPushArgumentExpressions.

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: sourceDescriptorAST
			 selector: #moveToReg:
			 arguments: (self rtlArgumentsForValue: aDRLoadStackValue result))
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitPush: aDRPush [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: 'ssPush',
				 (aDRPush operand1 rtlPushOperandQualifierForInstruction: aDRPush)
			 arguments: aDRPush operand1 rtlPushArgumentExpressions)
]
