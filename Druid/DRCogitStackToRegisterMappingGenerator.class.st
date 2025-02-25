Class {
	#name : 'DRCogitStackToRegisterMappingGenerator',
	#superclass : 'DRCogitSimpleStackGenerator',
	#instVars : [
		'markDeadCode'
	],
	#category : 'Druid-Cogit',
	#package : 'Druid',
	#tag : 'Cogit'
}

{ #category : 'helpers' }
DRCogitStackToRegisterMappingGenerator >> freeRegisterLiveMask: temporaryVariableNode [

	generatorMethodBuilder addStatement:
		(RBAssignmentNode variable: (RBVariableNode named: 'live') value: (RBMessageNode
				  receiver: (RBVariableNode named: 'live')
				  selector: #bitClear:
				  arguments: { (RBMessageNode
						   receiver: RBVariableNode selfNode
						   selector: #registerMaskFor:
						   arguments: { temporaryVariableNode copy }) }))
]

{ #category : 'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> generatePreambleForCFG: aDRControlFlowGraph [

	super generatePreambleForCFG: aDRControlFlowGraph.
	generatorMethodBuilder addVariableNamed: 'live'.
	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: 'live')
			 value: (RBLiteralValueNode value: 0))
]

{ #category : 'helpers' }
DRCogitStackToRegisterMappingGenerator >> generateRegisterAllocationFor: aDRResult inVariable: temporaryVariableNode [

	generatorMethodBuilder addVariableNamed: temporaryVariableNode name.
	generatorMethodBuilder addStatement:
		(RBAssignmentNode variable: temporaryVariableNode copy value: (RBMessageNode
				  receiver: RBVariableNode selfNode
				  selector: #allocateRegNotConflictingWith:ifNone:
				  arguments: {
						  (RBVariableNode named: 'live').
						  (RBVariableNode named: '[ ^ self unknownBytecode ]') })).
	generatorMethodBuilder addStatement: (self saveRegisterLiveMask: temporaryVariableNode)
]

{ #category : 'helpers' }
DRCogitStackToRegisterMappingGenerator >> initialize [

	super initialize.
	markDeadCode := false
]

{ #category : 'code-generation' }
DRCogitStackToRegisterMappingGenerator >> markAsDeadCode [

	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: 'deadCode')
			 value: (RBLiteralNode value: true)).
	markDeadCode := true
]

{ #category : 'helpers' }
DRCogitStackToRegisterMappingGenerator >> moveToReg: aRBMessageNode from: aDRInstruction [

	| temporaryVariableNode |
	temporaryVariableNode := self allocateVariable: aDRInstruction result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: aRBMessageNode
			 selector: #copyToReg:
			 arguments: { (RBVariableNode named: temporaryVariableNode) })
]

{ #category : 'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> preProcessBlock: aBasicBlock [

	(markDeadCode and: [ self hasDeferredBranchesTo: aBasicBlock ])
		ifTrue: [ "If a previous instruction marked dead code, we need to unmark it because we keep generating code"
			markDeadCode := false.
			generatorMethodBuilder addStatement: (RBAssignmentNode
					 variable: (RBVariableNode named: 'deadCode')
					 value: (RBLiteralNode value: false)) ].
	super preProcessBlock: aBasicBlock
]

{ #category : 'helpers' }
DRCogitStackToRegisterMappingGenerator >> saveRegisterLiveMask: temporaryVariableNode [

	^ RBAssignmentNode
		  variable: (RBVariableNode named: 'live')
		  value:
			  (RBMessageNode 
					receiver: (RBVariableNode named: 'live') 
					selector: #bitOr: 
					arguments: { (RBMessageNode
					    receiver: RBVariableNode selfNode
					    selector: #registerMaskFor:
					    arguments: { temporaryVariableNode copy }) })
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitBytecodeJump: aDRBytecodeJump [

	super visitBytecodeJump: aDRBytecodeJump.

	"Everything that follows an unconditional jump is potentially dead code"
	self markAsDeadCode
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitClosureCreation: aDRClosureCreation [
	"Create full closure intrinsic uses ReceiverResultReg, SendNumArgsReg and ClassReg."

	"Save registers live"
	| liveInit |
	liveInit := generatorMethodBuilder statements
		                 detect: [ :statement | statement isAssignment and: [ statement variable name = 'live' ] ]
		                 ifNone: [ self error: 'Missing variable #live initialization!' ].
	generatorMethodBuilder after: liveInit addStatement:
		(self saveRegisterLiveMask: (RBVariableNode named: DRPhysicalGeneralPurposeRegister receiverResultReg name)).
	generatorMethodBuilder after: liveInit addStatement:
		(self saveRegisterLiveMask: (RBVariableNode named: DRPhysicalGeneralPurposeRegister sendNumArgsReg name)).
	generatorMethodBuilder after: liveInit addStatement:
		(self saveRegisterLiveMask: (RBVariableNode named: DRPhysicalGeneralPurposeRegister classReg name)).

	"Free ReceiverResultReg value"
	generatorMethodBuilder addStatement:
		(RBMessageNode receiver: RBVariableNode selfNode selector: #voidReceiverResultRegContainsSelf).

	"Allocate registers for call the trampoline"
	generatorMethodBuilder addStatement:
		(RBMessageNode receiver: RBVariableNode selfNode selector: #ssAllocateCallReg:and:and: arguments: {
				 (RBVariableNode named: DRPhysicalGeneralPurposeRegister receiverResultReg name).
				 (RBVariableNode named: DRPhysicalGeneralPurposeRegister sendNumArgsReg name).
				 (RBVariableNode named: DRPhysicalGeneralPurposeRegister classReg name) }).

	"Create the Closure (saved in ReceiverResultReg)"
	super visitClosureCreation: aDRClosureCreation.

	"Free auxiliar registers"
	self freeRegisterLiveMask: (RBVariableNode named: DRPhysicalGeneralPurposeRegister sendNumArgsReg name).
	self freeRegisterLiveMask: (RBVariableNode named: DRPhysicalGeneralPurposeRegister classReg name)
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitCogitSendMarshall: aDRCogitSendMarshall [
	"self marshallSendArguments: numArgs."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
				 #marshallSendArguments , (aDRCogitSendMarshall shouldPop
					  ifTrue: [ #'' ]
					  ifFalse: [ 'NoPush' ]) , ':'
			 arguments:
				 (aDRCogitSendMarshall numberOfArguments
					  rtlPushArgumentExpressions: self))
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitDeoptimize: aDRDeoptimize [

	super visitDeoptimize: aDRDeoptimize.
	markDeadCode := true.
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitFlushStack: aDRFlushStack [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssFlushStackExceptTop:
			 arguments: { (RBLiteralValueNode value: aDRFlushStack operand1 value) })
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitJITMessageSend: aDRMessageSend [
	"self genMarshalledSend: selectorIndex numArgs: numArgs sendTable: aSendTable"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genMarshalledSendNoPush:numArgs:sendTable:
			 arguments: {
				(aDRMessageSend operand1 rtlPushArgumentExpressions: self) first.
				(aDRMessageSend operand2 rtlPushArgumentExpressions: self) first.
				(RBVariableNode named: aDRMessageSend sendTable value) })
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitLoadReceiver: aDRLoadReceiver [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ensureReceiverResultRegContainsSelf).

	self twoOperandMoveCogitRTL: aDRLoadReceiver operands: {
			(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg').
			aDRLoadReceiver }
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitLoadStackValue: aDRLoadStackValue [
	"(self ssDescriptorAt: 0) copyToReg: TempReg."

	| sourceDescriptorAST |
	sourceDescriptorAST := RBMessageNode
		                       receiver: RBVariableNode selfNode
		                       selector: #ssValue:
		                       arguments:
		                       (aDRLoadStackValue operand1
			                        rtlPushArgumentExpressions: self).

	self moveToReg: sourceDescriptorAST from: aDRLoadStackValue
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitPop: aDRPop [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssPop:popSpilled:
			 arguments: {
					 (RBLiteralValueNode value: aDRPop numberOfPoppedElements).
					 (RBLiteralValueNode value: aDRPop isUnspilled not) })
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitPush: aDRPush [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: 'ssPush' , (aDRPush operand1 rtlPushOperandQualifierForInstruction: aDRPush)
			 arguments: (aDRPush operand1 rtlPushArgumentExpressions: self))
]

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [
	"self ssFlushUpThroughTemporaryVariable: index.
	self MoveR: reg
		Mw: (self frameOffsetOfTemporary: tempIndex)
		r: FPReg."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssFlushUpThroughTemporaryVariable:
			 arguments:
			 (aDRStoreTemporaryVariable operand1 rtlPushArgumentExpressions:
				  self)).

	super visitStoreTempVar: aDRStoreTemporaryVariable
]

{ #category : 'visiting' }
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

{ #category : 'visiting' }
DRCogitStackToRegisterMappingGenerator >> visitUnspillStackSlot: aDRUnspillStackSlot [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssUnspillStackSlotAt:
			 arguments:
			 { (RBLiteralValueNode value: aDRUnspillStackSlot operand1 value) })
]
