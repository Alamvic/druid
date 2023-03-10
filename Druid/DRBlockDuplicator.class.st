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

	| endInstruction endInstructionCopy |
	^ controlFlowGraph newBasicBlockWith: [ :newBlock |
		| instructionCopy |

		aBasicBlock predecessors do: [ :p | newBlock addPredecessor: p ].

		"Duplicate instructions"
		aBasicBlock instructions allButLast do: [ :instruction |
			instructionMap at: instruction put: (instructionCopy := instruction copy).
			instructionCopy clearUsers.
			instructionCopy operands do: [ :e |
				e addUser: instructionCopy.
			].
			newBlock addInstruction: instructionCopy.
		 ].
		endInstruction := aBasicBlock endInstruction.
		instructionMap at: endInstruction put: (endInstructionCopy := endInstruction copy).
		newBlock endInstruction: endInstructionCopy.
		newBlock endInstruction operands do: [ :e |
				e addUser: newBlock endInstruction.
		].
		self patchInstructionsIn: newBlock
	]
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
	"Rewrite all operands"

	aBasicBlock instructions do: [ :newInstruction |
		newInstruction users copy do: [ :user | | replacement |
			"If an user comes from another block, keep using it"
			replacement := instructionMap at: user ifAbsent: [ user ].
			newInstruction removeUser: user.
			newInstruction addUser: replacement.
		].
		newInstruction operands copy do: [ :operand | | replacement |
			"If an operand comes from another block, keep using it"
			replacement := instructionMap at: operand ifAbsent: [ operand ].
			newInstruction replaceOperand: operand by: replacement.
		]
	]
]
