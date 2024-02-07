Class {
	#name : #DRStager,
	#superclass : #DROptimisation,
	#instVars : [
		'cfgEdgeWorklist',
		'ssaWorklist',
		'visited'
	],
	#category : #'Druid-Staging'
}

{ #category : #adding }
DRStager >> addBlockToWorklist: aDRBasicBlock [

	(visited includes: aDRBasicBlock) ifTrue: [ ^ self ].
	visited add: aDRBasicBlock.

	ssaWorklist addAll:
		(aDRBasicBlock instructions select: [ :e |
			 e isStaged not or: [ e isJump "continue tracking the blocks" ] ])
]

{ #category : #accessing }
DRStager >> applyTo: cfg [

	"double worklist algorithm. One block worklist, one instruction worklist.
	Re-work instruction dependents as soon as their instruction is treated.
	
	This should probably later be improved into a lattice
	 - top (we don't know)
	 - stageable
	 - bottom (we know it's not stageable)"
	cfgEdgeWorklist add: cfg initialBasicBlock.

	[ cfgEdgeWorklist isEmpty and: [ ssaWorklist isEmpty ] ] whileFalse: [
		[ ssaWorklist isEmpty ] whileFalse: [
			| nextInstruction |
			nextInstruction := ssaWorklist removeFirst.
			self stageInstruction: nextInstruction ].
		cfgEdgeWorklist notEmpty ifTrue: [
			self addBlockToWorklist: cfgEdgeWorklist removeFirst ] ]
]

{ #category : #staging }
DRStager >> canStage: anInstruction [

	^ self canStage: anInstruction modulo: Set new
]

{ #category : #staging }
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
		^ (anInstruction result isVirtualRegister or: [
			   anInstruction result isStagedRegister ]) and: [
			  self canStageOneLevel: anInstruction operand1 ] ].
	anInstruction isJITCompileTimeExpression ifTrue: [ ^ true ].
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
		^ anInstruction operands allSatisfy: [ :e |
			  self
				  canStage: e
				  modulo:
				  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ].

	anInstruction isLoadReceiver ifTrue: [ ^ false ].
	anInstruction isLoadStackValue ifTrue: [ ^ false ].

	"Bytecode jumps SHOULD be staged, their parameters cannot be registers"
	anInstruction class = DRBytecodeJump ifTrue: [ ^ true ].
	anInstruction class = DRContinueNextBytecode ifTrue: [ ^ false ].
	anInstruction isAnnotateBytecode ifTrue: [ ^ true ].
	anInstruction isCall ifTrue: [ ^ false ].
	anInstruction class = DRUnsignedCoercion ifTrue: [
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

	^ false
	"(anInstruction isArithmeticInstruction or: [ anInstruction isCopy ])
		ifFalse: [ ^ false ].

	^ anInstruction operands allSatisfy: [ :e | self isStaged: e ]"
]

{ #category : #testing }
DRStager >> canStageOneLevel: anOperand [

	anOperand isStaged ifTrue: [ ^ true ].
	anOperand isConstanteable ifTrue: [ ^ true ].
	^ false
]

{ #category : #adding }
DRStager >> initialize [

	super initialize.
	cfgEdgeWorklist := OrderedCollection new.
	ssaWorklist := OrderedCollection new.
	visited := Set new.
]

{ #category : #testing }
DRStager >> isStaged: anOperand [

	(self canStageOneLevel: anOperand) ifTrue: [ ^ true ].
	^ anOperand isStaged
]

{ #category : #testing }
DRStager >> isStaged: anOperand modulo: aSetOfPotentiallyStagedInstructions [

	"Returns true if the operand could be staged if we assume that the given set is staged"

	(self canStageOneLevel: anOperand) ifTrue: [ ^ true ].
	anOperand isMemoryAddress ifTrue: [
		^ anOperand operands allSatisfy: [ :e | self canStageOneLevel: e ] ].
	^ false
]

{ #category : #staging }
DRStager >> stageInstruction: anInstruction [
	"Just staged a push instruction"

	(anInstruction isPush and: [
		 anInstruction operand1 isMemoryAddress and: [ anInstruction operand1 base isStaged ] ])
		ifTrue: [
			| copy |
			"If we have found a push instruction whose address' base is staged, we must promote the value to runtime.
			We do so by inserting a non-staged copy.
			This copy will be register allocated and treat the staged value as a constant"
			copy := DRCopy
				        operands: { anInstruction operand1 base }
				        result: anInstruction instructionFactory allocateTemporaryRegister.
			anInstruction addBefore: copy.
			anInstruction replaceDependency: anInstruction operand1 base by: copy.
			^ self ].


	(self canStage: anInstruction) ifTrue: [
		anInstruction markAsStaged.

		"Maybe the users want to change their decisions"
		anInstruction users
			select: [ :e | e isStaged not and: [ (ssaWorklist includes: e) not ] ]
			thenDo: [ :e | ssaWorklist add: e ] ].

	anInstruction isJump ifTrue: [ anInstruction targets do: [ :t | cfgEdgeWorklist add: t ] ]
]
