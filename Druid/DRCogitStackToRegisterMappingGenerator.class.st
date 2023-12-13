Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitSimpleStackGenerator,
	#instVars : [
		'nextVariableIndex',
		'markDeadCode'
	],
	#category : #'Druid-Cogit'
}

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> allocateVariable: aDRResult [

	| temporaryVariableNode variableIndex |
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

	temporaryVariableNode := RBVariableNode named:
		                         't' , variableIndex asString.
	variables at: aDRResult put: temporaryVariableNode name.

	generatorMethodBuilder addVariableNamed: temporaryVariableNode name.
	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: temporaryVariableNode copy
			 value: (RBMessageNode
					  receiver: RBVariableNode selfNode
					  selector: #allocateRegNotConflictingWith:ifNone:
					  arguments: {
							  (RBVariableNode named: 'live').
							  (RBVariableNode named: '[ ^ self unknownBytecode ]') })).
	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: 'live')
			 value: (RBMessageNode
					  receiver: (RBVariableNode named: 'live')
					  selector: #bitOr:
					  arguments: { (RBMessageNode
							   receiver: RBVariableNode selfNode
							   selector: #registerMaskFor:
							   arguments: { temporaryVariableNode copy }) })).

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

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> twoOperandMoveCogitRTL: anInstruction operands: operands [

	self allocateVariable: anInstruction result.
	operands first isObjectReferenceValue ifTrue: [
		"It's moving a constant to a register. This constant could be an oop, so we need to annotate it."
		^ self generateMessage: #genMoveConstant:R: operands: operands ].

	^ super twoOperandMoveCogitRTL: anInstruction operands: operands
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
DRCogitStackToRegisterMappingGenerator >> visitClosureCreation: aDRClosureCreation [

	| allocatedTemporary |
	allocatedTemporary := self allocateVariable:
		                      aDRClosureCreation result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
			 #genCreateFullClosureInIndex:numCopied:ignoreContext:contextNumArgs:large:inBlock:intoRegister:
			 arguments: (aDRClosureCreation operands collect: [ :e |
					  (e rtlPushArgumentExpressions: self) first ]) , {
					 (RBVariableNode named:
						  '(coInterpreter methodNeedsLargeContext: methodObj)').
					 (RBVariableNode named: 'inBlock') }
				 , { (RBVariableNode named: allocatedTemporary) })
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
				aDRLoadReceiver result }
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
