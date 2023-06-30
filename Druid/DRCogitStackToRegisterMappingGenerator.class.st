Class {
	#name : #DRCogitStackToRegisterMappingGenerator,
	#superclass : #DRCogitCodeGenerator,
	#instVars : [
		'nextVariableIndex',
		'variables',
		'markDeadCode'
	],
	#category : #'Druid-Cogit'
}

{ #category : #helpers }
DRCogitStackToRegisterMappingGenerator >> allocateVariable: aVariableName [

	| temporaryVariableNode variableIndex |
	aVariableName isNoResult ifTrue: [ ^ self ].
	variables at: aVariableName ifPresent: [ :var | ^ var ].

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
	variables at: aVariableName put: temporaryVariableNode name.

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
DRCogitStackToRegisterMappingGenerator >> generatePostambleForCFG: aDRControlFlowGraph [

	"Generate the continuation jumps to the following code"
	self generateBlockIncomingJumps: #nextBytecode.

	super generatePostambleForCFG: aDRControlFlowGraph
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
	variables := Dictionary new.
	markDeadCode := false
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
DRCogitStackToRegisterMappingGenerator >> twoOperandMoveCogitRTL: anInstruction operands: operands [

  self allocateVariable: anInstruction result.
	(self selectorCogitRTL: '' operands: operands instruction: anInstruction) = #Cq:R: ifTrue: [ 
		"It's moving a constant to a register. This constant could be an oop, so we need to annotate it."
		self flag: #TOTHINK. "Is this strategy ok?"
		^ self generateMessage: #genMoveConstant:R: operands: operands ].

	^ super twoOperandMoveCogitRTL: anInstruction operands: operands
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitBytecodeJump: aDRBytecodeJump [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genJumpTo:
			 arguments:
			 { (self jitCompileTimeExpression: aDRBytecodeJump operand1) }).
	
	"Everything that follows an unconditional jump is potentially dead code"
	markDeadCode := true
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitCall: aDRCall [
	"self CallRT: ceSendMustBeBooleanTrampoline"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #CallRT:
			 arguments: { (RBVariableNode named: aDRCall operand1) }).
	aDRCall isMapped ifTrue: [ "self annotateBytecode: self Label"
		generatorMethodBuilder addStatement: (RBMessageNode
				 receiver: RBVariableNode selfNode
				 selector: #annotateBytecode:
				 arguments: { (RBMessageNode
						  receiver: RBVariableNode selfNode
						  selector: #Label
						  arguments: #(  )) }) ]
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

	self twoOperandMoveCogitRTL: aDRLoadFramePointer operands: {
			(DRPhysicalGeneralPurposeRegister name: 'FPReg').
			aDRLoadFramePointer result }
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
	self twoOperandMoveCogitRTL: aDRReturn operands: {
				aDRReturn operands first.
				(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg') }.

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
