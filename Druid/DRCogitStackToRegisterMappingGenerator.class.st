Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitCodeGenerator,
	#category : #'Druid-Cogit'
}

{ #category : #'as yet unclassified' }
DRCogitStackToRegisterMappingGenerator >> moveToReg: aRBMessageNode from: aDRInstruction [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: aRBMessageNode
			 selector: #moveToReg:
			 arguments: (self rtlArgumentsForValue: aDRInstruction result))
]

{ #category : #'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> returnNodeForCompiledMethod [

	^ RBLiteralNode value: 0
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitBytecodeJump: aDRBytecodeJump [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genJumpTo:
			 arguments:
			 { (self jitCompileTimeExpression: aDRBytecodeJump operand1) })
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitCall: aDRCall [

	"self CallRT: ceSendMustBeBooleanTrampoline"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #CallRT:
			 arguments: { (RBVariableNode named: aDRCall operand1) })
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitCogitSendMarshall: aDRCogitSendMarshall [

	"self marshallSendArguments: numArgs."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #marshallSendArguments:
			 arguments:
			 { (RBLiteralValueNode value:
				  aDRCogitSendMarshall numberOfArguments) })
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitContinueNextBytecode: aDRContinueNextBytecode [ 
	
	"This is a continuation to the next bytecode.
	If this is the last basic block, do nothing.
	Otherwise, make a jump to the end"
	self isLastBasicBlock ifTrue: [ ^ self ].
	
	"Jump to the end!"
	self
		generateJump: #Jump:
		target: nil
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitJITMessageSend: aDRMessageSend [

	"self genMarshalledSend: selectorIndex numArgs: numArgs sendTable: ordinarySendTrampolines"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genMarshalledSend:numArgs:sendTable:
			 arguments: { 
				RBLiteralValueNode value: aDRMessageSend cogitSelectorIndex.
				RBLiteralValueNode value: aDRMessageSend numberOfArguments.
				RBVariableNode named: 'ordinarySendTrampolines' }).
]

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
		                       selector: #ssValue:
		                       arguments:
		                       aDRLoadStackValue operand1
			                       rtlPushArgumentExpressions.

	self moveToReg: sourceDescriptorAST from: aDRLoadStackValue
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitPop: aDRPop [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssPop:
			 arguments: { (RBLiteralValueNode value: aDRPop numberOfPoppedElements) })
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitPopMany: aDRPopMany [ 

	^ self visitPop: aDRPopMany
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitPush: aDRPush [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: 'ssPush',
				 (aDRPush operand1 rtlPushOperandQualifierForInstruction: aDRPush)
			 arguments: aDRPush operand1 rtlPushArgumentExpressions)
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitReturn: aDRReturn [
	"Generate the postamble in the return cases"

	self generatePostambleForCFG: aDRReturn controlFlowGraph.

	"Move the return value to the receiver register result"
	self
		twoOperandCogitRTL: #Move
		operands: {
				aDRReturn operands first.
				(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg') }
		instruction: aDRReturn.

	"Then return"
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genUpArrowReturn)
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [
	"self simStackAt: index."

	| sourceDescriptorAST |
	sourceDescriptorAST := RBMessageNode
		                       receiver: RBVariableNode selfNode
		                       selector: #simStackTempAt:
		                       arguments: aDRLoadTemporaryVariable operand1 rtlPushArgumentExpressions.

	self moveToReg: sourceDescriptorAST from: aDRLoadTemporaryVariable
]
