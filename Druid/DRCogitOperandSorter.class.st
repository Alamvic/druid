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
	"If the first operand is a constant, extract it in a copy.
	This is useful for instructions that do nto accept constants as second operand."
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
DRCogitOperandSorter >> visitAnnnotateBytecode: aDRAnnotateBytecode [ 

	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBitAnd: aDRBitAnd [ 
	
	^ self visitCommutativeInstruction: aDRBitAnd
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBitOr: aDRBitOr [ 
	
	^ self visitCommutativeInstruction: aDRBitOr
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBitXor: aDRBitXor [ 
	
	^ self visitCommutativeInstruction: aDRBitXor
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBranchIfCondition: aDRBranchIfCondition [

	"JIT expressions do not need extras copies"
	aDRBranchIfCondition isJITCompileTimeExpression ifTrue: [ ^ self ].

	"Mask test conditions accept constants in cogit"
	(aDRBranchIfCondition condition isTestMask or: [
		 aDRBranchIfCondition condition isNoTestMask ]) ifTrue: [ ^ self ].


	"Cogit only accept constants as first operands (second in DruidIR).
	Equals comparison being commutative, we can reverse the operands to avoid an extra copy"
	(aDRBranchIfCondition operand1 isConstant and: [
		 aDRBranchIfCondition condition isEqualsThanComparison ]) ifTrue: [
		aDRBranchIfCondition reverseOperands ].

	self extractFirstOperandInCopyIfNecessaryFrom: aDRBranchIfCondition
]

{ #category : #visiting }
DRCogitOperandSorter >> visitBytecodeJump: aDRBytecodeJump [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitCall: aDRCall [ 
	
	^ nil
]

{ #category : #visiting }
DRCogitOperandSorter >> visitClosureActivation: aDRClosureActivation [
	"Nothing"

	
]

{ #category : #visiting }
DRCogitOperandSorter >> visitCommutativeInstruction: anInstruction [

	anInstruction operand1 isInstruction ifFalse: [ 
		anInstruction reverseOperands ].
	self extractFirstOperandInCopyIfNecessaryFrom: anInstruction
]

{ #category : #visiting }
DRCogitOperandSorter >> visitContinueNextBytecode: aDRContinueNextBytecode [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitCopy: aDRCopy [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitDivision: aDRDivision [ 
	
	"Division only exist between two registers in cogit. Extract the second operand too"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRDivision.
	aDRDivision reverseOperands.
	self extractFirstOperandInCopyIfNecessaryFrom: aDRDivision.
	aDRDivision reverseOperands.
]

{ #category : #visiting }
DRCogitOperandSorter >> visitFloatToIntConversion: aDRFloatToInt [ 
	
	"Int to float conversion is on registers only"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRFloatToInt
]

{ #category : #visiting }
DRCogitOperandSorter >> visitFlushStack: aDRFlushStack [
	"Nothing"

	
]

{ #category : #visiting }
DRCogitOperandSorter >> visitGetConditionCode: aDRGetConditionCode [

	(aDRGetConditionCode condition isCommutative and: [ 
		 aDRGetConditionCode operand1 isInstruction not ]) ifTrue: [ 
		aDRGetConditionCode reverseOperands ].
	self extractFirstOperandInCopyIfNecessaryFrom: aDRGetConditionCode
]

{ #category : #visiting }
DRCogitOperandSorter >> visitGetFloatBits: aDRGetFloatBits [ 

	"Get float bits are only from registers..."
	self extractFirstOperandInCopyIfNecessaryFrom: aDRGetFloatBits
]

{ #category : #visiting }
DRCogitOperandSorter >> visitGetFloatFromBits: aDRGetFloatFromBits [ 

	"Get float bits are only from registers..."
	self extractFirstOperandInCopyIfNecessaryFrom: aDRGetFloatFromBits
]

{ #category : #visiting }
DRCogitOperandSorter >> visitIntToFloatConversion: aDRIntToFloat [ 
	
	"Int to float conversion is on registers only"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRIntToFloat
]

{ #category : #visiting }
DRCogitOperandSorter >> visitJITMessageSend: aDRMessageSend [ 

	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitJump: aDRJump [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLeftShift: aDRLeftShift [ 
	
	"Shift is not commutative!
	=> extract its first operand into a copy instruction"
	self extractFirstOperandInCopyIfNecessaryFrom: aDRLeftShift
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoad: aDRLoad [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoadFramePointer: aDRLoadFramePointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoadReceiver: aDRLoadReceiver [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoadStackPointer: aDRLoadStackPointer [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitLoadStackValue: aDRLoadStackValue [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitMod: aDRMod [ 
	
	"Mod and division are the same instruction in cogit"
	self visitDivision: aDRMod
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
DRCogitOperandSorter >> visitPop: aDRPop [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitPopMany: aDRPopMany [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitPush: aDRPush [ 
	
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

	aDRStore isJITCompileTimeExpression ifTrue: [ ^ self ].
	
	"Stores at runtime are only from registers..."
	self extractFirstOperandInCopyIfNecessaryFrom: aDRStore
]

{ #category : #visiting }
DRCogitOperandSorter >> visitStoreTempVar: aDRStoreTemporaryVariable [ 
	
	"Nothing"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitSubstract: aDRSubtract [ 
	
	"Subtraction is not commutative!
	=> extract its first operand into a copy instruction"
	
	self extractFirstOperandInCopyIfNecessaryFrom: aDRSubtract
]

{ #category : #visiting }
DRCogitOperandSorter >> visitTemporaryVariable: aDRLoadTemporaryVariable [ 

	"Nothing?"
]

{ #category : #visiting }
DRCogitOperandSorter >> visitUnsignedCoercion: aDRUnsignedCoercion [ 
	
	"Nothing"
]
