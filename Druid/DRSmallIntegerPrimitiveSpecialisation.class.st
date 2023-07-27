Class {
	#name : #DRSmallIntegerPrimitiveSpecialisation,
	#superclass : #DRPrimitiveSpecialisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRSmallIntegerPrimitiveSpecialisation class >> pragmaName [

	^ #smallInteger
]

{ #category : #'generation-IR' }
DRSmallIntegerPrimitiveSpecialisation >> addConstraintForReceiver: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : #'generation-IR' }
DRSmallIntegerPrimitiveSpecialisation >> initialConstraint [
	"Since the specialisation check uses receiverTags, we need to add the constraint over the receiver by hand."

	^ DRRegisterDomain
		  mask: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg')
		  with: 1
]

{ #category : #'code-generation' }
DRSmallIntegerPrimitiveSpecialisation >> preamble [

	| preamble |
	preamble := DruidTestRTLCompiler lookupSelector: #smallIntegerSpecialisedPreamble.
	^ preamble ast body copy
]

{ #category : #'cog jit support' }
DRSmallIntegerPrimitiveSpecialisation >> smallIntegerTag [
	"Duplicated code"

	^ 1
]

{ #category : #api }
DRSmallIntegerPrimitiveSpecialisation >> specialiseIR: aDRPrimitiveIRGenerator [

	| conditionalJump conditionalBasicBlock |
	conditionalBasicBlock := aDRPrimitiveIRGenerator currentBasicBlock.

	"receiverTags = 1"
	conditionalJump := aDRPrimitiveIRGenerator
		                   instantiateNoResultInstruction: DRBranchIfCondition
		                   operands: {
				                   DREqualsThanComparison new.
				                   (DRJitCompileTimeExpression expression: 'receiverTags').
				                   self smallIntegerTag asDRValue }.
	conditionalBasicBlock endInstruction: conditionalJump.

	"if false, fail"
	conditionalJump falseBranch: aDRPrimitiveIRGenerator ir failureExitBlock.
	aDRPrimitiveIRGenerator ir failureExitBlock addPredecessor: conditionalBasicBlock.

	"if true, continue"
	conditionalJump trueBranch: aDRPrimitiveIRGenerator newBasicBlock.
	aDRPrimitiveIRGenerator currentBasicBlock addPredecessor: conditionalBasicBlock.


	"Add constraints for Receiver"
	self addConstraintForReceiver: aDRPrimitiveIRGenerator ir.

]
