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

{ #category : #'code-generation' }
DRCogitStackToRegisterMappingGenerator >> innerCompilePathFrom: aDRBasicBlock [

	| dominatorTree dominatedBlocks sharedPathToTheEnd truePath innerGenerator |
	"We should create a path from the branch to the end"
	dominatorTree := aDRBasicBlock controlFlowGraph buildDominatorTree.
	dominatedBlocks := aDRBasicBlock controlFlowGraph reversePostOrderBlocks select: [ :b |
		                   b isDominatedBy: aDRBasicBlock ].

	"Remove blocks only visited during inner compilation from outer"
	dominatedBlocks do: [ :b | blocks remove: b ].

	sharedPathToTheEnd := dominatorTree iteratedDominanceFrontierOf: aDRBasicBlock.
	sharedPathToTheEnd := blocks select: [ :e | 
		sharedPathToTheEnd anySatisfy: [ :potentialDominator |
			e isDominatedBy: potentialDominator ] ].
	truePath := dominatedBlocks , sharedPathToTheEnd.

	innerGenerator := self innerGenerator.
	innerGenerator blocks: truePath.
	innerGenerator generateCodeFromBlocks.
	innerGenerator generatePostambleForCFG: aDRBasicBlock controlFlowGraph.
	innerGenerator generateReturn.

	^ innerGenerator generatorMethodBuilder sequenceNode
]

{ #category : #copy }
DRCogitStackToRegisterMappingGenerator >> innerGenerator [

	| innerGenerator |
	innerGenerator := self copy.
	innerGenerator variables: variables copy.
	innerGenerator deferredBranches: deferredBranches copy.
	innerGenerator generatorMethodBuilder: DRGeneratorMethodBuilder new.
	^ innerGenerator
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
	operands first isObjectReferenceValue ifTrue: [
		"It's moving a constant to a register. This constant could be an oop, so we need to annotate it."
		^ self generateMessage: #genMoveConstant:R: operands: operands ].

	^ super twoOperandMoveCogitRTL: anInstruction operands: operands
]

{ #category : #accessing }
DRCogitStackToRegisterMappingGenerator >> variables [

	^ variables
]

{ #category : #accessing }
DRCogitStackToRegisterMappingGenerator >> variables: anObject [

	variables := anObject
]

{ #category : #visiting }
DRCogitStackToRegisterMappingGenerator >> visitBytecodeJump: aDRBytecodeJump [
	"self Jump: (self ensureFixupAt: targetBytecode)"

	| targetBytecode |
	targetBytecode := self jitCompileTimeExpression:
		                  aDRBytecodeJump operand1.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #Jump:
			 arguments: { (RBMessageNode
					  receiver: RBVariableNode selfNode
					  selector: #ensureFixupAt:
					  arguments: { targetBytecode }) }).

	"Everything that follows an unconditional jump is potentially dead code"
	markDeadCode := true.
	generatorMethodBuilder addStatement: (RBAssignmentNode
				 variable: (RBVariableNode named: 'deadCode')
				 value: (RBLiteralNode value: true))
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
DRCogitStackToRegisterMappingGenerator >> visitClosureCreation: aDRClosureCreation [

	| allocatedTemporary |
	allocatedTemporary := self allocateVariable:
		                      aDRClosureCreation result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
			 #genCreateFullClosure:numCopied:ignoreContext:contextNumArgs:large:inBlock:intoRegister:
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
DRCogitStackToRegisterMappingGenerator >> visitCompileTimeStore: aDRStore [

	| varName |
	varName := aDRStore address name.

	(varName beginsWith: 'r') ifTrue: [ "Temporary compile time variable"
		generatorMethodBuilder addVariableNamed: varName ].

	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: varName)
			 value: (self jitCompileTimeExpression: aDRStore operand1))
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
DRCogitStackToRegisterMappingGenerator >> visitDeoptimize: aDRDeoptimize [

	generatorMethodBuilder addStatement:
		(RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #deoptimize).
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
DRCogitStackToRegisterMappingGenerator >> visitLoadStackPointer: aDRLoadStackPointer [

	self twoOperandMoveCogitRTL: aDRLoadStackPointer operands: {
			(DRPhysicalGeneralPurposeRegister name: 'SPReg').
			aDRLoadStackPointer result }
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

	| isBlockReturn generator |
	self generatePostambleForCFG: aDRReturn controlFlowGraph.

	"Move the return value to the receiver register result"
	self twoOperandMoveCogitRTL: aDRReturn operands: {
				aDRReturn operands first.
				(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg') }.

	"Then return"
	isBlockReturn := aDRReturn operand2 value.
	generator := isBlockReturn
		ifTrue: [ #genBlockReturn ]
		ifFalse: [ #genUpArrowReturn ].
	
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: generator)
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
		 arguments: (aDRStoreTemporaryVariable operand1 rtlPushArgumentExpressions: self)).
	generatorMethodBuilder addStatement: (RBMessageNode
		 receiver: RBVariableNode selfNode
		 selector: #MoveR:Mw:r:
		 arguments: {
			self rtlExpressionForValue: aDRStoreTemporaryVariable result.
			RBMessageNode
				receiver: RBVariableNode selfNode
				selector: #frameOffsetOfTemporary:
				arguments: (aDRStoreTemporaryVariable operand1 rtlPushArgumentExpressions: self).
			RBVariableNode named: 'FPReg'
	}).
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
