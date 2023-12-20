Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitSimpleStackGenerator,
	#instVars : [
		'nextVariableIndex',
		'markDeadCode',
		'parent'
	],
	#category : #'Druid-Cogit'
}

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> allocateVariable: aDRResult [

	| temporaryVariableNode variableIndex |
	parent ifNotNil: [ ^ parent allocateVariable: aDRResult ].
	
	aDRResult isNoResult ifTrue: [ ^ self ].
	variables at: aDRResult ifPresent: [ :var | ^ var ].

	"Should generate a variable allocation in the lines of:
	
	live := 0.

	t0 := self allocateRegNotConflictingWith: live ifNone: [ ^ ABANDON COMPILATION ].
	live := live | t0.
	
	t1 := self allocateRegNotConflictingWith: live ifNone: [ ^ ABANDON COMPILATION ].
	live := live | t1."

	variableIndex := nextVariableIndex.
	nextVariableIndex := nextVariableIndex + 1.

	temporaryVariableNode := RBVariableNode named: 't' , variableIndex asString.
	variables at: aDRResult put: temporaryVariableNode name.

	generatorMethodBuilder addVariableNamed: temporaryVariableNode name.
	generatorMethodBuilder addStatement:
		(RBAssignmentNode variable: temporaryVariableNode copy value: (RBMessageNode
				  receiver: RBVariableNode selfNode
				  selector: #allocateRegNotConflictingWith:ifNone:
				  arguments: {
						  (RBVariableNode named: 'live').
						  (RBVariableNode named: '[ ^ self unknownBytecode ]') })).
	self saveRegisterLiveMask: temporaryVariableNode.

	^ temporaryVariableNode name
]

{ #category : #'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> generatePreambleForCFG: aDRControlFlowGraph [

	super generatePreambleForCFG: aDRControlFlowGraph.
	generatorMethodBuilder addVariableNamed: 'live'.
	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: 'live')
			 value: (RBLiteralValueNode value: 0))
]

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> initialize [

	super initialize.
	nextVariableIndex := 0.
	markDeadCode := false
]

{ #category : #'code-generation' }
DRCogitStackToRegisterMappingGenerator >> markAsDeadCode [

	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: 'deadCode')
			 value: (RBLiteralNode value: true)).
	markDeadCode := true
]

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> moveToReg: aRBMessageNode from: aDRInstruction [

	| temporaryVariableNode |
	temporaryVariableNode := self allocateVariable: aDRInstruction result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: aRBMessageNode
			 selector: #copyToReg:
			 arguments: { (RBVariableNode named: temporaryVariableNode) })
]

{ #category : #accessing }
DRCogitStackToRegisterMappingGenerator >> parent: aDRCogitGenerator [

	parent := aDRCogitGenerator
]

{ #category : #'ir-to-ast' }
DRCogitStackToRegisterMappingGenerator >> preProcessBlock: aBasicBlock [

	markDeadCode ifTrue: [ "If a previous instruction marked dead code, we need to unmark it because we keep generating code"
		markDeadCode := false.
		generatorMethodBuilder addStatement: (RBAssignmentNode
				 variable: (RBVariableNode named: 'deadCode')
				 value: (RBLiteralNode value: false)) ].
	super preProcessBlock: aBasicBlock
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> rtlExpressionForValue: aValue [

	variables at: aValue result ifPresent: [ :var | ^ RBVariableNode named: var ].
	^ super rtlExpressionForValue: aValue
]

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> saveRegisterLiveMask: temporaryVariableNode [

	generatorMethodBuilder addStatement:
		(RBAssignmentNode variable: (RBVariableNode named: 'live') value: (RBMessageNode
				  receiver: (RBVariableNode named: 'live')
				  selector: #bitOr:
				  arguments: { (RBMessageNode
						   receiver: RBVariableNode selfNode
						   selector: #registerMaskFor:
						   arguments: { temporaryVariableNode copy }) }))
]

{ #category : #accessing }
DRCogitStackToRegisterMappingGenerator >> variables [

	^ variables
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitBytecodeJump: aDRBytecodeJump [

	super visitBytecodeJump: aDRBytecodeJump.

	"Everything that follows an unconditional jump is potentially dead code"
	self markAsDeadCode
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitCall: aDRCall [
	"self CallRT: ceSendMustBeBooleanTrampoline"

	| rbCallNode |
	aDRCall operands size > 0 ifTrue: [
		generatorMethodBuilder addStatement: (RBMessageNode
				 receiver: RBVariableNode selfNode
				 selector: #MoveR:R:
				 arguments: {
						 (self rtlExpressionForValue: aDRCall operand1).
						 (RBVariableNode named: 'TempReg') }) ].
	rbCallNode := RBMessageNode
		              receiver: RBVariableNode selfNode
		              selector: #CallRT:
		              arguments:
		              { (RBVariableNode named: aDRCall functionName) }.
	aDRCall shouldSaveLinkReg
		ifTrue: [
			generatorMethodBuilder addStatement: (RBMessageNode
					 receiver: (RBVariableNode named: #backEnd)
					 selector: #saveAndRestoreLinkRegAround:
					 arguments:
					 { (RBBlockNode body: (RBSequenceNode statements: { rbCallNode })) }) ]
		ifFalse: [ generatorMethodBuilder addStatement: rbCallNode ].
	aDRCall isMapped ifTrue: [ 
		self assert: aDRCall controlFlowGraph hasAnnotatedBytecode  ]
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitClosureCreation: aDRClosureCreation [
	"Create full closure intrinsic uses ReceiverResultReg, SendNumArgsReg and ClassReg."

	| copy |
	"Save registers live"
	self flag: #TODO. "Validate that the register was not already taken"
	self saveRegisterLiveMask: (RBVariableNode named:
			 DRPhysicalGeneralPurposeRegister receiverResultReg name).
	self saveRegisterLiveMask: (RBVariableNode named:
			 DRPhysicalGeneralPurposeRegister sendNumArgsReg name).
	self saveRegisterLiveMask:
		(RBVariableNode named:
			 DRPhysicalGeneralPurposeRegister classReg name).

	"Free ReceiverResultReg value"
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #voidReceiverResultRegContainsSelf).

	"Allocate registers for call the trampoline"
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssAllocateCallReg:and:and:
			 arguments: {
					 (RBVariableNode named:
						  DRPhysicalGeneralPurposeRegister receiverResultReg name).
					 (RBVariableNode named:
						  DRPhysicalGeneralPurposeRegister sendNumArgsReg name).
					 (RBVariableNode named:
						  DRPhysicalGeneralPurposeRegister classReg name) }).

	super visitClosureCreation: aDRClosureCreation
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitCogitSendMarshall: aDRCogitSendMarshall [
	"self marshallSendArguments: numArgs."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #marshallSendArguments:
			 arguments:
				 (aDRCogitSendMarshall numberOfArguments
					  rtlPushArgumentExpressions: self))
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitDeoptimize: aDRDeoptimize [

	super visitDeoptimize: aDRDeoptimize.
	markDeadCode := true.
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitFlushStack: aDRFlushStack [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ssFlushStack)
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitJITMessageSend: aDRMessageSend [
	"self genMarshalledSend: selectorIndex numArgs: numArgs sendTable: aSendTable"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genMarshalledSend:numArgs:sendTable:
			 arguments: {
				(aDRMessageSend operand1 rtlPushArgumentExpressions: self) first.
				(aDRMessageSend operand2 rtlPushArgumentExpressions: self) first.
				(RBVariableNode named: aDRMessageSend sendTable value) })
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitLoadReceiver: aDRLoadReceiver [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #ensureReceiverResultRegContainsSelf).

  self allocateVariable: aDRLoadReceiver result.
	self twoOperandMoveCogitRTL: aDRLoadReceiver operands: { 
				(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg').
				aDRLoadReceiver }
]

{ #category : #visiting }
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
