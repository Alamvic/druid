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
			 e isStaged not ])
]

{ #category : #accessing }
DRStager >> applyTo: cfg [

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
	anInstruction isJITMessageSend ifTrue: [ ^ false ].
	anInstruction isClosureCreation ifTrue: [ ^ false ].
	anInstruction isConditionalBranch ifTrue: [
		^ anInstruction operands allSatisfy: [ :e | self isStaged: e ] ].
	anInstruction isJump ifTrue: [ ^ false ].
	anInstruction isReturn ifTrue: [ ^ false ].
	anInstruction isObjectReferenceCopy ifTrue: [ ^ true ].
	anInstruction isJITCompileTimeExpression ifTrue: [ ^ true ].
	anInstruction isCopy ifTrue: [
		^ self canStage: anInstruction operand1 ].
	anInstruction isArithmeticInstruction ifTrue: [
		^ anInstruction operands allSatisfy: [ :e |
			  self
				  canStage: e
				  modulo:
				  (aSetOfPotentiallyStagedInstructions copyWith: anInstruction) ] ].
	anInstruction isLoad ifTrue: [
		anInstruction address isObjectReferenceValue ifTrue: [ ^ true ].
		^ false ].
	anInstruction isStore ifTrue: [
		(anInstruction address isObjectReferenceValue and: [
			 self
				 canStage: anInstruction
				 modulo: aSetOfPotentiallyStagedInstructions ]) ifTrue: [ ^ true ].

		^ false ].
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
	anInstruction class = DRUnsignedCoercion ifTrue: [ ^ false ].
	anInstruction class = DRDeoptimize ifTrue: [ ^ false ].
	anInstruction isLoadStackPointer ifTrue: [ ^ false ].
	anInstruction isLoadTemporaryVariable ifTrue: [ ^ false ].
	1 halt
	"(anInstruction isArithmeticInstruction or: [ anInstruction isCopy ])
		ifFalse: [ ^ false ].

	^ anInstruction operands allSatisfy: [ :e | self isStaged: e ]"
]

{ #category : #testing }
DRStager >> canStageOneLevel: anOperand [

	anOperand isStaged ifTrue: [ ^ true ].
	anOperand isConstant ifTrue: [ ^ true ].
	anOperand isObjectReferenceValue ifTrue: [ ^ true ].
	anOperand isReallyJITCompileTimeExpression ifTrue: [ ^ true ].
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
	anOperand isMemoryAddress ifTrue: [
		^ anOperand operands allSatisfy: [ :e | self canStageOneLevel: e ] ].
	^ false
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

	(self canStage: anInstruction) ifTrue: [
		anInstruction markAsStaged.
		
		"Maybe the users want to change their decisions"
		anInstruction users
			select: [ :e |
				e isStaged not and: [ (ssaWorklist includes: e) not ] ]
			thenDo: [ :e | ssaWorklist add: e ] ].

	anInstruction isJump ifTrue: [
		anInstruction targets do: [ :t | cfgEdgeWorklist add: t ] ]
]
