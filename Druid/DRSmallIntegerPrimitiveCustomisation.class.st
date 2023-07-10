Class {
	#name : #DRSmallIntegerPrimitiveCustomisation,
	#superclass : #DRPrimitiveCustomisation,
	#category : #'Druid-CompilerCompiler'
}

{ #category : #accessing }
DRSmallIntegerPrimitiveCustomisation class >> pragmaName [

	^ #smallInteger
]

{ #category : #api }
DRSmallIntegerPrimitiveCustomisation >> generateCustomisedPreamble: aGeneratorMethodBuilder [
1halt.
"	aGeneratorMethodBuilder addStatement: self preamble"
]

{ #category : #'generation-IR' }
DRSmallIntegerPrimitiveCustomisation >> initialConstraint [

	^ DRRegisterDomain
		  mask: (DRPhysicalGeneralPurposeRegister name: 'ReceiverResultReg')
		  with: 1
]

{ #category : #api }
DRSmallIntegerPrimitiveCustomisation >> optimize: anIR [

	anIR initialConstraint: self initialConstraint
]

{ #category : #'code-generation' }
DRSmallIntegerPrimitiveCustomisation >> preamble [

	| preamble |
	preamble := DruidTestRTLCompiler lookupSelector: #smallIntegerSpecialisedPreamble.
	^ preamble ast body copy
]

{ #category : #'cog jit support' }
DRSmallIntegerPrimitiveCustomisation >> smallIntegerTag [
	"Duplicated code"

	^ 1
]

{ #category : #'generation-IR' }
DRSmallIntegerPrimitiveCustomisation >> specialiseIR: aDRPrimitiveIRGenerator [

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
]
