Class {
	#name : #DRBasicBlock,
	#superclass : #Object,
	#instVars : [
		'id',
		'instructions',
		'blockValue',
		'endInstruction',
		'predecessors',
		'origin',
		'temporaryFrame',
		'vmOperandStack',
		'primitiveFailureCode'
	],
	#category : #'Druid-IR'
}

{ #category : #accessing }
DRBasicBlock class >> id: anInteger [ 
	
	^ self new
		id: anInteger;
		yourself
]

{ #category : #accessing }
DRBasicBlock >> addInstruction: anInstruction [

	self haltIf: [ anInstruction isPhiFunction and:
		[ anInstruction operands size ~= self predecessors size ] ].
	anInstruction basicBlock: self.
	^ instructions add: anInstruction
]

{ #category : #accessing }
DRBasicBlock >> addPredecessor: aBasicBlock [

	predecessors add: aBasicBlock
]

{ #category : #accessing }
DRBasicBlock >> blockValue [
	
	^ blockValue
]

{ #category : #accessing }
DRBasicBlock >> blockValue: aValue [
	
	self haltIf: [ (aValue isKindOf: DRValue) not ].
	blockValue := aValue
]

{ #category : #accessing }
DRBasicBlock >> endInstruction [

	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> endInstruction: aDRInstruction [ 
	| previousSuccessors newSuccessors lostSuccessors |
	aDRInstruction basicBlock: self. 
	
	previousSuccessors := self successors.
	newSuccessors := aDRInstruction targets.
	lostSuccessors := previousSuccessors difference: newSuccessors.
	
	lostSuccessors do: [ :e |
		e removePredecessor: self. ].
	
	endInstruction := aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> firstInstruction [
	
	^ instructions first
]

{ #category : #testing }
DRBasicBlock >> hasFinalInstruction [
	
	^ endInstruction notNil and: [endInstruction isNoop not]
]

{ #category : #testing }
DRBasicBlock >> hasPredecessors [
	
	^ predecessors notEmpty
]

{ #category : #accessing }
DRBasicBlock >> id [

	^ id
]

{ #category : #accessing }
DRBasicBlock >> id: anObject [

	id := anObject
]

{ #category : #accessing }
DRBasicBlock >> initialize [
	super initialize.	
	instructions := OrderedCollection new.
	blockValue := DRNullValue new.
	predecessors := OrderedCollection new.

	endInstruction := DRNoop new.
	endInstruction basicBlock: self.
]

{ #category : #'as yet unclassified' }
DRBasicBlock >> inspectionInstructions: aBuilder [
	<inspectorPresentationOrder: 0 title: 'Instructions'> 
	
	^ aBuilder newList
		display: [ :each | StObjectPrinter asTruncatedTextFrom: each prettyPrint ];
		items: self instructions;
		yourself
]

{ #category : #accessing }
DRBasicBlock >> instructions [

	| allInstructions |
	allInstructions := OrderedCollection new.
	self instructionsDo: [ :i | allInstructions add: i ].
	^ allInstructions
]

{ #category : #iterating }
DRBasicBlock >> instructionsDo: aFullBlockClosure [ 

	instructions do: aFullBlockClosure.
	endInstruction ifNotNil: [ aFullBlockClosure value: endInstruction ]
]

{ #category : #building }
DRBasicBlock >> jumpIfTrueTo: aDRBasicBlock ifFalseTo: aDRBasicBlock2 [ 
	
	aDRBasicBlock addPredecessor: self.
	aDRBasicBlock2 addPredecessor: self.
	
	^ endInstruction := DRConditionalJump new
		trueBranch: aDRBasicBlock;
		falseBranch: aDRBasicBlock2;
		yourself
]

{ #category : #building }
DRBasicBlock >> jumpTo: aBasicBlock [ 
	
	self endInstruction: (DRJump new target: aBasicBlock).
	aBasicBlock addPredecessor: self.
	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ endInstruction
]

{ #category : #'operand stack' }
DRBasicBlock >> mergeOperandStacksIn: aBuilder [

	"We can only merge two operand stacks with the same number of elements"
	| stackSize newStack predecessorStacks phi predecessorFailures mergedPrimitiveFailure |
	
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	self predecessors ifEmpty: [ ^ self ].
	
	"If I have a single predecessor, just copy the operand stack"
	self predecessors size = 1 ifTrue: [
		primitiveFailureCode := self predecessors first primitiveFailureCode.
		^ vmOperandStack := self predecessors first vmOperandStack ].

	predecessorStacks := self predecessors collect: [ :e | e vmOperandStack ].
	self assert: (predecessorStacks collect: [ :e | e size ] as: Set) size = 1.
	
	predecessorFailures := self predecessors collect: [ :e | e primitiveFailureCode ].
	mergedPrimitiveFailure := (DRPhiFunction joinedVariables: predecessorFailures)
		result: aBuilder allocateTemporaryRegister;
		origin: thisContext stack;
		simplify.
	primitiveFailureCode := mergedPrimitiveFailure.
	mergedPrimitiveFailure isPhiFunction ifTrue: [ self addInstruction: mergedPrimitiveFailure ].
	
	stackSize := predecessorStacks first size.
	newStack := Stack new.
	1 to: stackSize do: [ :e | | values |
		values := predecessorStacks collect: [ :stack | stack at: e ].
		phi := (DRPhiFunction joinedVariables: values)
			result: aBuilder allocateTemporaryRegister;
			origin: thisContext stack;
			simplify.
		newStack push: phi.
		phi isPhiFunction ifTrue: [ self addInstruction: phi].
	].
	^ vmOperandStack := newStack
]

{ #category : #temporaries }
DRBasicBlock >> mergeTemporaryFramesIn: aBuilder [

	| temporaryNames newTemporaryFrame predecessorTemporaries |
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	self predecessors ifEmpty: [ ^ temporaryFrame ].
	
	predecessorTemporaries := self predecessors collect: [ :e | e temporaryFrame ].
	self assert: (predecessorTemporaries collect: [ :e | e size ] as: Set) size = 1.
	
	"Do not include the return temporary variable as it is a special variable that will be resolved at the end of the method"
	temporaryNames := predecessorTemporaries flatCollect: [ :e | e keys ] as: Set.
	newTemporaryFrame := Dictionary new.
	temporaryNames collect: [ :name | | values phi |
		name ~= #'__return' ifTrue: [
			values := self predecessors collect: [ :p | p temporaryAt: name ].
			phi := (DRPhiFunction joinedVariables: values)
				result: aBuilder allocateTemporaryRegister;
				origin: thisContext stack.
			newTemporaryFrame at: name put: phi.
			self addInstruction: phi ]
		ifFalse: [ 
			newTemporaryFrame at: name put: (self predecessors anyOne temporaryAt: #'__return').
			 ] ].
	
	^ temporaryFrame := newTemporaryFrame
]

{ #category : #accessing }
DRBasicBlock >> numberOfPredecessors [
	
	^ predecessors size
]

{ #category : #accessing }
DRBasicBlock >> origin: aCollection [ 
	origin := aCollection
]

{ #category : #accessing }
DRBasicBlock >> predecessors [
	^ predecessors
]

{ #category : #accessing }
DRBasicBlock >> primitiveFailureCode [
	^ primitiveFailureCode
]

{ #category : #accessing }
DRBasicBlock >> primitiveFailureCode: anInstruction [ 
	primitiveFailureCode := anInstruction
]

{ #category : #printing }
DRBasicBlock >> printOn: aStream [

	aStream nextPutAll: 'DRBasicBlock('.
	aStream print: id.
	aStream nextPutAll: ' => '.
	self successors size = 1
		ifTrue: [ aStream nextPutAll: self successors anyOne id asString ]
		ifFalse: [
			aStream nextPutAll: '{'.
			self successors
				do: [ :e | aStream nextPutAll: e id asString ]
				separatedBy: [ aStream nextPutAll: ' ,' ].
			aStream nextPutAll: '}'.
		].
	aStream nextPutAll: ')'
]

{ #category : #instructions }
DRBasicBlock >> removeInstruction: aDRInstruction [
	
	endInstruction = aDRInstruction
		ifTrue: [ ^ self endInstruction: DRNoop new ].
	
	instructions remove: aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> removePredecessor: aDRBasicBlock [ 
	
	predecessors remove: aDRBasicBlock
]

{ #category : #replacing }
DRBasicBlock >> replace: anInstruction by: aReplacementInstruction [
	
	instructions replaceAll: anInstruction with: aReplacementInstruction.
	endInstruction = anInstruction
		ifTrue: [ self endInstruction: aReplacementInstruction ]
]

{ #category : #accessing }
DRBasicBlock >> replaceInstruction: anInstruction by: anotherInstruction [ 

	anotherInstruction basicBlock: self.

	"Check if it is in the main list of instructions"
	(instructions includes: anInstruction) ifTrue: [ 
		^ instructions at: (instructions indexOf: anInstruction) put: anotherInstruction ].
	
	"Otherwise check if it is the block end instruction"
	endInstruction ~= anInstruction
		ifTrue: [ self error: 'This block does not contain the instruction' ].
	
	endInstruction := anotherInstruction
]

{ #category : #initialization }
DRBasicBlock >> setUpInitialFailureCode [
	self shouldBeImplemented.
]

{ #category : #accessing }
DRBasicBlock >> successor [
	
	| successors |
	successors := self successors.
	successors size ~= 1 ifTrue: [ self error: 'This block does not contain a single successor' ].
	^ successors first
]

{ #category : #accessing }
DRBasicBlock >> successors [
	
	^ endInstruction targets
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString [ 
	
	^ temporaryFrame at: aString
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString put: aValue [ 
	
	self haltIf: [ (aValue isKindOf: DRValue) not ].
	temporaryFrame at: aString put: aValue
]

{ #category : #accessing }
DRBasicBlock >> temporaryFrame [
	^ temporaryFrame
]

{ #category : #accessing }
DRBasicBlock >> temporaryFrame: aTemporaryFrame [ 
	temporaryFrame := aTemporaryFrame
]

{ #category : #accessing }
DRBasicBlock >> vmOperandStack [

	^ vmOperandStack
]

{ #category : #accessing }
DRBasicBlock >> vmOperandStack: anObject [

	vmOperandStack := anObject
]
