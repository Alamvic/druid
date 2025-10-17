Class {
	#name : 'DRTreegraphBytecodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'builder',
		'methodName',
		'numArgs',
		'workingListBlocks'
	],
	#category : 'Druid-Opal-BytecodeGeneration',
	#package : 'Druid-Opal',
	#tag : 'BytecodeGeneration'
}

{ #category : 'cfg-to-ir' }
DRTreegraphBytecodeGenerator >> generateAllBlocks [

	[ workingListBlocks isEmpty ] whileFalse: [
		self generateBytecodesForNextBlock ]
]

{ #category : 'cfg-to-ir' }
DRTreegraphBytecodeGenerator >> generateBranch: aDRBranch [

	| lastBlockInOneBranch |
	self flag: #TOCHECK. "Always ifTrue: ?"
	builder addLiteral: #ifTrue:.

	builder jumpAheadTo: #else if: true.
	self assert: aDRBranch falseBranch = self nextBlock.
	[
	lastBlockInOneBranch := self nextBlock.
	self generateBytecodesForNextBlock ] doWhileFalse: [
		self hasNextBlock and: [
			self nextBlock hasPredecessor: aDRBranch basicBlock ] ].
	builder jumpAheadTo: #end.

	builder jumpAheadTarget: #else.
	self assert: aDRBranch trueBranch = self nextBlock.
	[ self generateBytecodesForNextBlock ] doWhileFalse: [
		self hasNextBlock and: [
			self nextBlock hasPredecessor: lastBlockInOneBranch ] ].
	builder jumpAheadTarget: #end
]

{ #category : 'cfg-to-ir' }
DRTreegraphBytecodeGenerator >> generateBytecodeForInstruction: anInstruction [

	| nDup |
	anInstruction operands do: [ :op |
		op isInstruction ifFalse: [ "It is a leaf in the tree not yet visited"
			op acceptVisitor: self ] ].

	anInstruction acceptVisitor: self.

	self flag: #FIXME. "This implementation does not work with branches because assume 
	all users will use the value (and with branches this is not always true) "
	nDup := anInstruction users size - 1.
	nDup timesRepeat: [ builder pushDup ] 
]

{ #category : 'cfg-to-ir' }
DRTreegraphBytecodeGenerator >> generateBytecodesForBlock: aDRBasicBlock [

	aDRBasicBlock instructionsDo: [ :i |
		self generateBytecodeForInstruction: i ]
]

{ #category : 'cfg-to-ir' }
DRTreegraphBytecodeGenerator >> generateBytecodesForNextBlock [

	self generateBytecodesForBlock: workingListBlocks removeFirst
]

{ #category : 'API' }
DRTreegraphBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	workingListBlocks := aDRControlFlowGraph reversePostOrderBlocks.
	self generateAllBlocks
]

{ #category : 'testing' }
DRTreegraphBytecodeGenerator >> hasNextBlock [

	^ workingListBlocks isNotEmpty
]

{ #category : 'initialization' }
DRTreegraphBytecodeGenerator >> initialize [

	builder := IRBuilder new.
	numArgs := 0.

]

{ #category : 'accessing' }
DRTreegraphBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : 'accessing' }
DRTreegraphBytecodeGenerator >> nextBlock [

	^ workingListBlocks first
]

{ #category : 'accessing' }
DRTreegraphBytecodeGenerator >> numArgs: anInteger [

	numArgs := anInteger
]

{ #category : 'ir-to-target' }
DRTreegraphBytecodeGenerator >> sendMessage: aString fromInstruction: aDRAdd [ 
	self shouldBeImplemented.
]

{ #category : 'building' }
DRTreegraphBytecodeGenerator >> targetAST [

	| method ir |
	ir := builder ir.
	ir numArgs: numArgs.
	method := ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitAddition: aDRAdd [

	builder send: #+
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitBranchIfCondition: aDRBranchIfCondition [

	aDRBranchIfCondition condition
		acceptVisitor: self
		withBranch: aDRBranchIfCondition.

	self generateBranch: aDRBranchIfCondition
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitConstant: aDRConstantValue [

	aDRConstantValue value isLiteral ifFalse: [
		aDRConstantValue value isGlobalVariable ifTrue: [
			^ builder pushLiteralVariable: aDRConstantValue value ].
		aDRConstantValue value isClassType ifTrue: [
			^ builder pushLiteralVariable: aDRConstantValue value binding ] ].

	builder pushLiteral: aDRConstantValue value
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitCopy: aDRCopy [ 

	"Operand was already pushed, so do nothing"
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRGetConditionCode [ 

	builder send: #=
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitGetConditionCode: aDRGetConditionCode [

	aDRGetConditionCode condition
		acceptVisitor: self
		withBranch: aDRGetConditionCode
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitInterpreterValue: aDRInterpreterValue [ 

	self flag: #TODO. "It is always self ?"
	builder pushReceiver
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitJump: aDRJump [ 

	"Fall directly"
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [

	builder addTemp: aDRLoadArgument argName.
	builder pushTemp: aDRLoadArgument argName
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitLoadInstVar: aDRLoadInstanceVariable [

	builder send: #instVarAt:
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitMessageSend: aDRMessageSend [

	builder send: aDRMessageSend selector
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitMultiply: aDRMultiply [ 

	builder send: #*
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [ 

	"Everything is resolved in the branch?"
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitReturn: aDRReturn [

	builder returnTop
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitSubstract: aDRSubtract [

	builder send: #-
]

{ #category : 'visiting' }
DRTreegraphBytecodeGenerator >> visitTypeOf: aDRTypeOf inBranch: aDRBranchIfCondition [ 

	builder send: #isMemberOf:
]
