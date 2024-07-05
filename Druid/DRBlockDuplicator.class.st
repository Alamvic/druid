Class {
	#name : #DRBlockDuplicator,
	#superclass : #Object,
	#instVars : [
		'controlFlowGraph',
		'instructionMap',
		'blockMap'
	],
	#category : #'Druid-IR'
}

{ #category : #'editing keys' }
DRBlockDuplicator >> basicDuplicateBlock: aBasicBlock [

	^ controlFlowGraph newBasicBlockWith: [ :newBlock |
		  | endInstruction endInstructionCopy |
		  aBasicBlock predecessors do: [ :p | newBlock addPredecessor: p ].

		  "Duplicate instructions"
		  aBasicBlock instructions allButLast do: [ :instruction |
			  | instructionCopy |
			  instructionMap at: instruction put: (instructionCopy := instruction copy).
			  "New intructions have no users"
			  instructionCopy clearUsers.
			  "Dependecies have a new user"
			  instructionCopy dependencies do: [ :e | e addUser: instructionCopy ].
			  newBlock addInstruction: instructionCopy ].

		  "Duplicate end instruction"
		  endInstruction := aBasicBlock endInstruction.
		  instructionMap at: endInstruction put: (endInstructionCopy := endInstruction copy).
		  newBlock endInstruction: endInstructionCopy.
		  newBlock endInstruction operands do: [ :e | e addUser: newBlock endInstruction ].

		  self patchInstructionsIn: newBlock ]
]

{ #category : #accessing }
DRBlockDuplicator >> controlFlowGraph [

	^ controlFlowGraph
]

{ #category : #accessing }
DRBlockDuplicator >> controlFlowGraph: anObject [

	controlFlowGraph := anObject
]

{ #category : #'editing keys' }
DRBlockDuplicator >> duplicate: blocks [

	blocks do: [ :b | 
		blockMap at: b put: (self basicDuplicateBlock: b) ].
	blockMap valuesDo: [ :b | self patchInstructionsIn: b ].
	blockMap valuesDo: [ :b | self patchControlFlowIn: b ].
	^ blockMap
]

{ #category : #'editing keys' }
DRBlockDuplicator >> initialize [
	super initialize.
	instructionMap := Dictionary new.
	blockMap := Dictionary new.
]

{ #category : #'editing keys' }
DRBlockDuplicator >> patchControlFlowIn: aBasicBlock [

	aBasicBlock predecessors copy do: [ :p | 
		| newPredecessor |
		newPredecessor := blockMap at: p ifAbsent: [ p ].
		aBasicBlock replacePredecessor: p by: newPredecessor ].
	aBasicBlock successors copy do: [ :p | 
		| newSuccessor |
		newSuccessor := blockMap at: p ifAbsent: [ p ].
		aBasicBlock replaceSuccessor: p by: newSuccessor ]
]

{ #category : #'editing keys' }
DRBlockDuplicator >> patchInstructionsIn: aBasicBlock [
	"Replace all users and dependencies of block instructions using the instructionMap"
	aBasicBlock instructions do: [ :newInstruction |
		newInstruction users copy do: [ :user | | replacement |
			"If an user comes from another block, keep using it"
			replacement := instructionMap at: user ifAbsent: [ user ].
			newInstruction removeUser: user.
			newInstruction addUser: replacement.
		].
		newInstruction dependencies copy do: [ :operand | | replacement |
			"If an operand comes from another block, keep using it"
			replacement := instructionMap at: operand ifAbsent: [ operand ].
			newInstruction replaceDependency: operand by: replacement.
		]
	]
]
