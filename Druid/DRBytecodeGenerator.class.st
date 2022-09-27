Class {
	#name : #DRBytecodeGenerator,
	#superclass : #Object,
	#instVars : [
		'builder',
		'methodName',
		'numArgs',
		'currentBasicBlock',
		'predecessor'
	],
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #'ir-to-ast' }
DRBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	self visitBlock: aDRControlFlowGraph initialBasicBlock.
]

{ #category : #'ir-to-ast' }
DRBytecodeGenerator >> initialize [

	builder := IRBuilder new.
	numArgs := 0
]

{ #category : #accessing }
DRBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : #accessing }
DRBytecodeGenerator >> numArgs: anInteger [

	numArgs := anInteger
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> sendMessage: selector fromInstruction: aDRInstruction [

	aDRInstruction operands do: [ :o | o acceptVisitor: self ].
	builder send: selector
]

{ #category : #building }
DRBytecodeGenerator >> targetAST [

	| method ir |
	ir := builder ir.
	ir numArgs: numArgs.
	method := ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitAddition: aDRAdd [

	self sendMessage: #+ fromInstruction: aDRAdd
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBlock: aDRBasicBlock [

	predecessor := currentBasicBlock.
	currentBasicBlock := aDRBasicBlock.
	aDRBasicBlock instructionsDo: [ :i | i acceptVisitor: self ]
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBranchIfCondition: aDRConditionalBranch [

	aDRConditionalBranch condition acceptVisitor: self withBranch: aDRConditionalBranch.

	builder jumpAheadTo: #else if: false.
	self visitBlock: aDRConditionalBranch trueBranch.
	builder jumpAheadTo: #end.
	builder jumpAheadTarget: #else.
	self visitBlock: aDRConditionalBranch falseBranch.
	builder jumpAheadTarget: #end
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitBranchIfTrue: aDRBranchIfTrue [ 

	aDRBranchIfTrue operand1 acceptVisitor: self.

	builder jumpAheadTo: #else if: false.
	self visitBlock: aDRBranchIfTrue trueBranch.
	builder jumpAheadTo: #end.
	builder jumpAheadTarget: #else.
	self visitBlock: aDRBranchIfTrue falseBranch.
	builder jumpAheadTarget: #end
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitConstant: aDRConstantValue [ 

	builder pushLiteral: aDRConstantValue value
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitCopy: aDRCopy [

	aDRCopy operand1 acceptVisitor: self
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitEqualsThan: aDREqualsThanComparison inBranch: aDRBranchIfCondition [

	self sendMessage: #= fromInstruction: aDRBranchIfCondition
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitGetConditionCode: aDRGetConditionCode [ 

	aDRGetConditionCode condition acceptVisitor: self withBranch: aDRGetConditionCode
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitJump: aDRJump [

	self visitBlock: aDRJump target
	
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitLoadArgument: aDRLoadArgument [ 

	builder addTemp: aDRLoadArgument argName.
	builder pushTemp: aDRLoadArgument argName
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitMultiply: aDRMultiply [

	self sendMessage: #* fromInstruction: aDRMultiply
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitNoop: aDRNoop [

	"Do nothing"

	
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitPhiFunction: aDRPhiFunction [

	| resolved |
	resolved := aDRPhiFunction valuesForPredecessor: predecessor.
	resolved unique acceptVisitor: self
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitPhysicalRegister: aDRPhysicalRegister [

	self flag: #TODO. "Remove physical registers, use LoadReceiver / LoadArgument instructions"
	aDRPhysicalRegister name = 'ReceiverResultReg' ifTrue: [ 
		^ builder pushThisContext ].
	1 halt
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder returnTop.
]
