Class {
	#name : #DRCogitOperandSorter,
	#superclass : #Object,
	#category : #'Druid-Cogit'
}

{ #category : #applying }
DRCogitOperandSorter >> applyTo: aDRControlFlowGraph [ 
	
	aDRControlFlowGraph instructionsDo: [ :e | e acceptVisitor: self ]
]

{ #category : #visiting }
DRCogitOperandSorter >> extractFirstOperandInCopyIfNecessaryFrom: anInstruction [ 
	
	| copy |
	anInstruction operand1 isInstruction ifFalse: [
		copy := anInstruction instructionFactory copy: anInstruction operand1.
		anInstruction addBefore: copy.
		anInstruction replaceOperand: anInstruction operand1 by: copy
	]
]

{ #category : #visiting }
DRCogitOperandSorter >> visitAddition: aDRAdd [ 
	
	self visitCommutativeInstruction: aDRAdd
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBranchIfCondition: aDRBranchIfCondition [ 

	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitCommutativeInstruction: anInstruction [

	anInstruction operand1 isInstruction not
		ifTrue: [ anInstruction reverseOperands ].
	self extractFirstOperandInCopyIfNecessaryFrom: anInstruction
]

{ #category : #visiting }
DRCogitOperandSorter >> visitCopy: aDRCopy [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitDivision: aDRDivision [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitGetConditionCode: aDRGetConditionCode [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitJump: aDRJump [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoad: aDRLoad [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitMod: aDRMod [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitMultiply: aDRMultiply [ 
	
	"Multiply only exist between two registers in cogit. Extract the second operand too"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRMultiply.
	aDRMultiply reverseOperands.
	self extractFirstOperandInCopyIfNecessaryFrom: aDRMultiply.
]

{ #category : #visiting }
DRCogitOperandSorter >> visitNegate: aDRNegate [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitNoop: aDRNoop [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitPhiFunction: aDRPhiFunction [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitReturn: aDRReturn [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitRightShift: aDRRightShift [ 
	
	"Shift is not commutative!
	=> extract its first operand into a copy instruction"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRRightShift
]

{ #category : #visiting }
DRCogitOperandSorter >> visitStore: aDRStore [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitSubstract: aDRSubtract [ 
	
	"Subtraction is not commutative!
	=> extract its first operand into a copy instruction"
	
	self extractFirstOperandInCopyIfNecessaryFrom: aDRSubtract
]
