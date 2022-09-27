Class {
	#name : #DRBytecodeGenerator,
	#superclass : #Object,
	#instVars : [
		'builder',
		'methodName',
		'numArgs'
	],
	#category : #'Druid-BytecodeGeneration'
}

{ #category : #'ir-to-ast' }
DRBytecodeGenerator >> generateTargetASTFromIR: aDRControlFlowGraph methodName: selector [

	aDRControlFlowGraph reversePostOrderBlocks do: [ :block | 
		block instructionsDo: [ :i | i acceptVisitor: self ] ].
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

	self sendMessage:  #+ fromInstruction: aDRAdd
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
DRBytecodeGenerator >> visitJump: aDRJump [

	"Simple jumps do nothing for now, continue next instructions"
	
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
DRBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder returnTop.
]
