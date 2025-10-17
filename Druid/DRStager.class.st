Class {
	#name : 'DRStager',
	#superclass : 'DROptimisation',
	#instVars : [
		'cfgEdgeWorklist',
		'ssaWorklist',
		'visited'
	],
	#category : 'Druid-Staging',
	#package : 'Druid',
	#tag : 'Staging'
}

{ #category : 'staging' }
DRStager >> canStage: anInstruction modulo: aSetOfPotentiallyStagedInstructions [

	(aSetOfPotentiallyStagedInstructions includes: anInstruction)
		ifTrue: [ ^ true ].
	(self canStageOneLevel: anInstruction) ifTrue: [ ^ true ].
	anInstruction isNullValue ifTrue: [ ^ false ].
	anInstruction isJITMessageSend ifTrue: [ ^ false ].
	anInstruction isClosureCreation ifTrue: [ ^ false ].
	anInstruction isConditionalBranch ifTrue: [
		^ anInstruction operands allSatisfy: [ :e | self isStaged: e ] ].
	anInstruction isJump ifTrue: [ ^ false ].
	anInstruction isReturn ifTrue: [ ^ false ].
	anInstruction isCopy ifTrue: [
		^ (anInstruction result isSSARegister or: [
			   anInstruction result isStagedRegister ]) and: [
			  self canStageOneLevel: anInstruction operand1 ] ].
	(anInstruction isArithmeticInstruction or: [ anInstruction isNegate ])
		ifTrue: [
			^ anInstruction operands allSatisfy: [ :e |
				  self
					  canStage: e
					  modulo:
					  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ].
	anInstruction isLoad ifTrue: [ ^ false ].
	anInstruction isStore ifTrue: [ ^ false ].
	anInstruction isStackInstruction ifTrue: [ ^ false ].
	anInstruction isSetConditionCode ifTrue: [
		^ anInstruction operands allSatisfy: [ :e |
			  self
				  canStage: e
				  modulo:
				  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ].
	anInstruction isPhiFunction ifTrue: [
		^ (anInstruction hasRecursiveUse or: [
			   anInstruction basicBlock dominator lastInstruction isStaged ])
			  and: [
				  anInstruction operands allSatisfy: [ :e |
					  self
						  canStage: e
						  modulo:
						  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ] ].

	anInstruction isLoadReceiver ifTrue: [ ^ false ].
	anInstruction isLoadStackValue ifTrue: [ ^ false ].

	"Bytecode jumps SHOULD be staged, their parameters cannot be registers"
	anInstruction class = DRBytecodeJump ifTrue: [ ^ true ].
	anInstruction class = DRContinueNextBytecode ifTrue: [ ^ false ].
	anInstruction isAnnotateBytecode ifTrue: [ ^ true ].
	anInstruction isCall ifTrue: [ ^ false ].
	({ DRUnsignedCoercion . DRSignedCoercion } includes: anInstruction class) ifTrue: [
		^ anInstruction operands allSatisfy: [ :e |
			  self
				  canStage: e
				  modulo:
				  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ].
	anInstruction class = DRDeoptimize ifTrue: [ ^ false ].
	anInstruction isLoadStackPointer ifTrue: [ ^ false ].
	anInstruction isLoadTemporaryVariable ifTrue: [ ^ false ].
	anInstruction isLoadArgument ifTrue: [ ^ false ].
	anInstruction isLoadFramePointer ifTrue: [ ^ false ].
	anInstruction isNoop ifTrue: [ ^ false ].
	anInstruction isCogitSendMarshall ifTrue: [ ^ false ].
	(anInstruction isKindOf: DRFlushStack) ifTrue: [ ^ false ].
	anInstruction isJITCompileTimeExpression ifTrue: [ ^ true ].
	^ false
	"(anInstruction isArithmeticInstruction or: [ anInstruction isCopy ])
		ifFalse: [ ^ false ].

	^ anInstruction operands allSatisfy: [ :e | self isStaged: e ]"
]
