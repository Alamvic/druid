Class {
	#name : #DRBytecodeGenerator,
	#superclass : #Object,
	#instVars : [
		'builder',
		'methodName'
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

	builder := IRBuilder new
]

{ #category : #accessing }
DRBytecodeGenerator >> methodName: aString [

	methodName := aString
]

{ #category : #building }
DRBytecodeGenerator >> targetAST [

	| method |
	method := builder ir compiledMethod.
	method selector: methodName.
	^ method
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitAddition: aDRAdd [

	aDRAdd operands do: [ :o | o acceptVisitor: self ].
	builder send: #+
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitConstant: aDRConstantValue [ 

	builder pushLiteral: aDRConstantValue value
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitJump: aDRJump [

	"Simple jumps do nothing for now, continue next instructions"
	
]

{ #category : #'ir-to-target' }
DRBytecodeGenerator >> visitReturn: aDRReturn [ 

	aDRReturn operand1 acceptVisitor: self.
	builder returnTop.
]
