Class {
	#name : #DRArrayPrimitiveSpecialisation,
	#superclass : #DRPrimitiveSpecialisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #api }
DRArrayPrimitiveSpecialisation >> addInitialConstraint: aDRPrimitiveControlFlowGraph [
	"Do nothing, let the inference to the added checks"

	"aDRPrimitiveControlFlowGraph initialConstraint: self initialConstraint"
]

{ #category : #'header formats' }
DRArrayPrimitiveSpecialisation >> arrayFormat [
	"Duplicated code"

	^ 2
]

{ #category : #formatting }
DRArrayPrimitiveSpecialisation >> formatMask [
	"Duplicated code"

	^ 16r1f
]

{ #category : #formatting }
DRArrayPrimitiveSpecialisation >> formatShift [
	"Duplicated code"

	^ 24
]

{ #category : #'generation-IR' }
DRArrayPrimitiveSpecialisation >> initialConstraint [

	^ DRRegisterDomain
		  reg: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg')
		  constraint: (DROopConstraint withType: Array)
]

{ #category : #api }
DRArrayPrimitiveSpecialisation >> specialiseIR: aDRPrimitiveIRGenerator [

	| conditionalJump conditionalBasicBlock receiverRegister |
	receiverRegister := aDRPrimitiveIRGenerator receiverRegister.
	aDRPrimitiveIRGenerator pushFrame: DRStackFrame new.
	
	conditionalBasicBlock := aDRPrimitiveIRGenerator currentBasicBlock.
	"ReceiverResultReg MASK 3 (isImmediate:)"
	conditionalJump := aDRPrimitiveIRGenerator
		                   instantiateNoResultInstruction: DRBranchIfCondition
		                   operands: {
				                   DRTestMask new.
				                   receiverRegister.
				                   self tagMask asDRValue }.
	conditionalBasicBlock endInstruction: conditionalJump.

	"if true, fail"
	conditionalJump trueBranch: aDRPrimitiveIRGenerator ir failureExitBlock.
	aDRPrimitiveIRGenerator ir failureExitBlock addPredecessor: conditionalBasicBlock.

	"if false, continue"
	conditionalJump falseBranch: aDRPrimitiveIRGenerator newBasicBlock.
	aDRPrimitiveIRGenerator currentBasicBlock addPredecessor: conditionalBasicBlock.

	"Load base header"
	aDRPrimitiveIRGenerator addInstructionFrom: nil instructionKind: DRLoad operands: {
			8 asDRValue.
			receiverRegister }.

	"Get format from header"
	aDRPrimitiveIRGenerator
		addInstructionFrom: nil
		instructionKind: DRRightShift
		operands: {
				aDRPrimitiveIRGenerator popOperand. "base header"
				self formatShift asDRValue }.
				
	aDRPrimitiveIRGenerator addInstructionFrom: nil instructionKind: DRBitAnd operands: {
			aDRPrimitiveIRGenerator popOperand. "base header shifted"
			self formatMask asDRValue }.

	"Format = 2 (isArray)"
	conditionalBasicBlock := aDRPrimitiveIRGenerator currentBasicBlock.
	conditionalJump := aDRPrimitiveIRGenerator
		                   instantiateNoResultInstruction: DRBranchIfCondition
		                   operands: {
				                   DREqualsThanComparison new.
				                   aDRPrimitiveIRGenerator popOperand. "format"
				                   self arrayFormat asDRValue }.
	conditionalBasicBlock endInstruction: conditionalJump.
	
	"if false, fail"
	conditionalJump falseBranch: aDRPrimitiveIRGenerator ir failureExitBlock.
	aDRPrimitiveIRGenerator ir failureExitBlock addPredecessor: conditionalBasicBlock.

	"if true, continue"
	conditionalJump trueBranch: aDRPrimitiveIRGenerator newBasicBlock.
	aDRPrimitiveIRGenerator currentBasicBlock addPredecessor: conditionalBasicBlock.

	aDRPrimitiveIRGenerator popFrameMergingDeferredReturns
]

{ #category : #'word size' }
DRArrayPrimitiveSpecialisation >> tagMask [
	"Duplicated code"

	^ 7
]
