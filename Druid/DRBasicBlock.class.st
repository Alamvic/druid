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
		'vmOperandStack'
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

	anInstruction block: self. 
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
	aDRInstruction block: self. 
	endInstruction := aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> firstInstruction [
	
	^ instructions first
]

{ #category : #testing }
DRBasicBlock >> hasFinalInstruction [
	
	^ endInstruction notNil
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
	
	| jump |
	endInstruction ifNotNil: [ self error: 'Cannot override an end instruction' ].
	endInstruction := (jump := DRJump new target: aBasicBlock).
	jump block: self. 
	aBasicBlock addPredecessor: self.
	^ endInstruction
]

{ #category : #accessing }
DRBasicBlock >> lastInstruction [
	
	^ endInstruction
]

{ #category : #building }
DRBasicBlock >> markAsDeadCode [
	
	"Put a symbol to mark this block is dead code.
	It has no predecessors, so it will never be iterated in the CFG"
	endInstruction := #deadCode
]

{ #category : #'operand stack' }
DRBasicBlock >> mergeOperandStacksIn: aBuilder [

	"We can only merge two operand stacks with the same number of elements"
	| stackSize newStack predecessorStacks phi |
	
	"If I have no predecessors, I'm very probably dead code.
	No need to merge anything"
	self predecessors ifEmpty: [ ^ self ].
	
	"If I have a single predecessor, just copy the operand stack"
	self predecessors size = 1 ifTrue: [
		^ vmOperandStack := self predecessors first vmOperandStack ].
	
	predecessorStacks := self predecessors collect: [ :e | e vmOperandStack ].
	self assert: (predecessorStacks collect: [ :e | e size ] as: Set) size = 1.
	
	stackSize := predecessorStacks first size.
	newStack := Stack new.
	1 to: stackSize do: [ :e | | values |
		values := predecessorStacks collect: [ :stack | stack at: e ].
		phi := (DRPhiFunction joinedVariables: values)
			result: aBuilder allocateTemporaryRegister;
			originBlocks: self predecessors;
			origin: thisContext stack;
			yourself.
		newStack push: phi.
		self addInstruction: phi.
	].
	^ vmOperandStack := newStack
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
	
	instructions remove: aDRInstruction
]

{ #category : #accessing }
DRBasicBlock >> replaceInstruction: anInstruction by: anotherInstruction [ 

	instructions at: (instructions indexOf: anInstruction) put: anotherInstruction.
]

{ #category : #accessing }
DRBasicBlock >> successors [
	
	endInstruction ifNil: [ ^ #() ].
	endInstruction = #deadCode
		ifTrue: [ ^ #() ].
	^ endInstruction targets
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString [ 
	
	^ temporaryFrame at: aString
]

{ #category : #temporaries }
DRBasicBlock >> temporaryAt: aString put: aValue [ 
	
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
