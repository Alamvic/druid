Class {
	#name : #DRCogitSimpleStackGenerator,
	#superclass : #DRCogitCodeGenerator,
	#instVars : [
		'variables'
	],
	#category : #'Druid-Cogit'
}

{ #category : #'ir-to-ast' }
DRCogitSimpleStackGenerator >> generatePostambleForCFG: aDRControlFlowGraph [

	"Generate the continuation jumps to the following code"
	self generateBlockIncomingJumps: #nextBytecode.

	super generatePostambleForCFG: aDRControlFlowGraph
]

{ #category : #initialization }
DRCogitSimpleStackGenerator >> initialize [

	super initialize.
	variables := Dictionary new
]

{ #category : #copy }
DRCogitSimpleStackGenerator >> innerGenerator [

	| innerGenerator |
	innerGenerator := self copy.
	innerGenerator variables: variables copy.
	innerGenerator deferredBranches: deferredBranches copy.
	innerGenerator generatorMethodBuilder: DRGeneratorMethodBuilder new.
	^ innerGenerator
]

{ #category : #'ir-to-ast' }
DRCogitSimpleStackGenerator >> returnNodeForCompiledMethod [

	^ RBLiteralNode value: 0
]

{ #category : #accessing }
DRCogitSimpleStackGenerator >> variables: anObject [

	variables := anObject
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitAnnnotateBytecode: aDRAnnotateBytecode [
	"self annotateBytecode: self Label"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #annotateBytecode:
			 arguments: { (RBMessageNode
					  receiver: RBVariableNode selfNode
					  selector: #Label
					  arguments: #(  )) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitBytecodeJump: aDRBytecodeJump [
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
					  arguments: { targetBytecode }) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitCall: aDRCall [
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
DRCogitSimpleStackGenerator >> visitClosureCreation: aDRClosureCreation [

	| allocatedTemporary |
	allocatedTemporary := self allocateVariable:
		                      aDRClosureCreation result.
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
			 #genCreateFullClosureInIndex:numCopied:ignoreContext:contextNumArgs:large:inBlock:intoRegister:
			 arguments: (aDRClosureCreation operands collect: [ :e |
					  (e rtlPushArgumentExpressions: self) first ]) , {
					 (RBVariableNode named: 'methodOrBlockNumArgs').
					 (RBVariableNode named:
						  '(coInterpreter methodNeedsLargeContext: methodObj)').
					 (RBVariableNode named: 'inBlock') }
				 , { (RBVariableNode named: allocatedTemporary) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitCompileTimeStore: aDRStore [

	| varName |
	varName := aDRStore address name.

	(varName beginsWith: 'r') ifTrue: [ "Temporary compile time variable"
		generatorMethodBuilder addVariableNamed: varName ].

	generatorMethodBuilder addStatement: (RBAssignmentNode
			 variable: (RBVariableNode named: varName)
			 value: (self jitCompileTimeExpression: aDRStore operand1))
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitContinueNextBytecode: aDRContinueNextBytecode [ 
	
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
DRCogitSimpleStackGenerator >> visitDeoptimize: aDRDeoptimize [

	generatorMethodBuilder addStatement:
		(RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #deoptimize)
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitJITMessageSend: aDRMessageSend [
	"self genMarshalledSend: selectorIndex numArgs: numArgs sendTable: aSendTable"

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #genSend:numArgs:sendTable:
			 arguments: {
				(aDRMessageSend operand1 rtlPushArgumentExpressions: self) first.
				(aDRMessageSend operand2 rtlPushArgumentExpressions: self) first.
				(RBVariableNode named: aDRMessageSend sendTable value) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitLoadFramePointer: aDRLoadFramePointer [

	self twoOperandMoveCogitRTL: aDRLoadFramePointer operands: {
			(DRPhysicalGeneralPurposeRegister name: 'FPReg').
			aDRLoadFramePointer result }
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitLoadReceiver: aDRLoadReceiver [

	generatorMethodBuilder addStatement:
		(RBVariableNode named: 'needsFrame ifTrue:
		[self putSelfInReceiverResultReg].').

	self allocateVariable: aDRLoadReceiver result.
	self twoOperandMoveCogitRTL: aDRLoadReceiver operands: {
			(DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg').
			aDRLoadReceiver result }
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitLoadStackPointer: aDRLoadStackPointer [

	self twoOperandMoveCogitRTL: aDRLoadStackPointer operands: {
			(DRPhysicalGeneralPurposeRegister name: 'SPReg').
			aDRLoadStackPointer result }
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitLoadStackValue: aDRLoadStackValue [
	"cogit MoveMw: 0 r: SPReg R: TempReg"
	self haltIf: [ aDRLoadStackValue operand1 value > 0 ].
	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #MoveMw:r:R:
			 arguments: {
					 (aDRLoadStackValue operand1 rtlPushArgumentExpressions: self) first.
					 (RBVariableNode named: 'SPReg').
					 (RBVariableNode named: aDRLoadStackValue result name) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitPop: aDRPop [

	aDRPop numberOfPoppedElements > 1 ifTrue: [ self halt ].

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #PopR:
			 arguments: { (RBVariableNode named: aDRPop result name) })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitPush: aDRPush [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector:
			 'Push'
			 , (aDRPush operand1 rtlOperandQualifierForInstruction: aDRPush)
			 arguments: (aDRPush operand1 rtlPushArgumentExpressions: self))
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitReturn: aDRReturn [
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
DRCogitSimpleStackGenerator >> visitStoreTempVar: aDRStoreTemporaryVariable [

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #MoveR:Mw:r:
			 arguments: {
					 (self rtlExpressionForValue: aDRStoreTemporaryVariable result).
					 (RBMessageNode
						  receiver: RBVariableNode selfNode
						  selector: #frameOffsetOfTemporary:
						  arguments:
						  (aDRStoreTemporaryVariable operand1
							   rtlPushArgumentExpressions: self)).
					 (RBVariableNode named: 'FPReg') })
]

{ #category : #visiting }
DRCogitSimpleStackGenerator >> visitTemporaryVariable: aDRLoadTemporaryVariable [ 

	"self MoveMw: (self frameOffsetOfTemporary: index) r: FPReg R: TempReg."

	generatorMethodBuilder addStatement: (RBMessageNode
			 receiver: RBVariableNode selfNode
			 selector: #MoveMw:r:R:
			 arguments: {
					 (RBMessageNode
						  receiver: RBVariableNode selfNode
						  selector: #frameOffsetOfTemporary:
						  arguments:
						  (aDRLoadTemporaryVariable operand1 rtlPushArgumentExpressions: self)).
					 (RBVariableNode named: 'FPReg').
					 (RBVariableNode named: aDRLoadTemporaryVariable result name) })
]
