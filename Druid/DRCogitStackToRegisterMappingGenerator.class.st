Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitCodeGenerator,
	#instVars : [
		'nextVariableIndex',
		'variables'
	],
	#category : #'Druid-Cogit'
}

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> allocateVariable: aVariableName [

	| temporaryVariableNode variableIndex |
	
	aVariableName isNoResult ifTrue: [ ^ self ].
	variables at: aVariableName ifPresent: [ :var | ^ var ].
	
	variableIndex := nextVariableIndex.
	nextVariableIndex := nextVariableIndex + 1.
	
	temporaryVariableNode := RBVariableNode named: 't', variableIndex asString.
	variables at: aVariableName put: temporaryVariableNode name.
	
	generatorMethodBuilder addVariableNamed: temporaryVariableNode name.
	"freeReg := self allocateRegNotConflictingWith: 0."
	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: temporaryVariableNode copy
			 value: (RBMessageNode
					  receiver: RBVariableNode selfNode
					  selector: #allocateRegNotConflictingWith:
					  arguments: { (RBLiteralValueNode value: 0) })).

	^ temporaryVariableNode name
]

{ #category : #'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> generatePostambleForCFG: aDRControlFlowGraph [

	"Generate the continuation jumps to the following code"
	self generateBlockIncomingJumps: #nextBytecode.

	super generatePostambleForCFG: aDRControlFlowGraph
]

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> initialize [

	super initialize.
	nextVariableIndex := 0.
	variables := Dictionary new
]

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> moveToReg: aRBMessageNode from: aDRInstruction [

	| temporaryVariableNode |
	temporaryVariableNode := self allocateVariable: aDRInstruction result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: aRBMessageNode
			 selector: #moveToReg:
			 arguments: { (RBVariableNode named: temporaryVariableNode) })
]

{ #category : #'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> returnNodeForCompiledMethod [

	^ RBLiteralNode value: 0
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> rtlExpressionForValue: aValue [

	variables at: aValue result ifPresent: [ :var | ^ RBVariableNode named: var ].
	^ super rtlExpressionForValue: aValue
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> twoOperandCogitRTL: aMnemonic operands: operands instruction: anInstruction [

	self allocateVariable: anInstruction result.
	super
		twoOperandCogitRTL: aMnemonic
		operands: operands
		instruction: anInstruction
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
		target: #nextBytecode
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

	self allocateVariable: aDRLoadReceiver result.
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
		                       (aDRLoadStackValue operand1
			                        rtlPushArgumentExpressions: self).

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
			 selector: 'ssPush'
				 ,
				 (aDRPush operand1 rtlPushOperandQualifierForInstruction: aDRPush)
			 arguments: (aDRPush operand1 rtlPushArgumentExpressions: self))
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
		                       arguments:
		                       (aDRLoadTemporaryVariable operand1
			                        rtlPushArgumentExpressions: self).

	self moveToReg: sourceDescriptorAST from: aDRLoadTemporaryVariable
]
