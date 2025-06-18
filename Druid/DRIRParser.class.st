Class {
	#name : 'DRIRParser',
	#superclass : 'Object',
	#instVars : [
		'input',
		'cfg',
		'blockMapping',
		'instrMapping',
		'initialBlock',
		'blockLabelRegex',
		'instructionRegex',
		'instrDependencies',
		'blockPredecessorsOrder',
		'opcodeToInstrMap'
	],
	#category : 'Druid-IR',
	#package : 'Druid',
	#tag : 'IR'
}

{ #category : 'instance creation' }
DRIRParser class >> for: inputString [

	^ self new initializeFor: inputString
]

{ #category : 'adding' }
DRIRParser >> addInstr: opcode to: aDRBasicBlock withResult: result andOperands: operands [

	| instrSubclasses instrClass instr |
	instrSubclasses := DRInstruction allSubclasses.

	instrClass := self resolveInstruction: opcode.
	instr := instrClass new.

	result = '_' ifFalse: [
		instr result: aDRBasicBlock controlFlowGraph instructionFactory
				allocateTemporaryRegister.
		instrMapping
			at: result
			ifPresent: [
			self error: 'Attempting to redefine variable ' , result ]
			ifAbsentPut: [ instr ] ].

	instrDependencies at: instr put: (instrClass
			 getDependenciesFromOperandsString: operands
			 withOpcode: opcode).

	aDRBasicBlock addInstruction: instr
]

{ #category : 'parsing' }
DRIRParser >> fixDependencies [

	instrDependencies keysAndValuesDo: [ :instruction :dependencies |
		dependencies first ifNotNil: [ :operands |
			instruction operands: (operands collect: [ :op |
					 instrMapping
						 at: op
						 ifPresent: [ :mapping | mapping ]
						 ifAbsent: [
							 self assert: op isString not description: 'Invalid operand'.
							 op ] ]) ].
		dependencies second ifNotNil: [ :targets |
			| targetBlocks |
			targetBlocks := targets collect: [ :target |
				                blockMapping
					                at: target
					                ifAbsent: [
					                self error: 'Invalid target: ' , target ] ].
			instruction targets: targetBlocks.
			targetBlocks do: [ :target |
				(target hasPredecessor: instruction basicBlock) ifFalse: [
					target addPredecessor: instruction basicBlock ] ] ] ].

	self fixPredecessors
]

{ #category : 'parsing' }
DRIRParser >> fixPredecessors [

	blockPredecessorsOrder keysAndValuesDo: [ :blockName :predOrder |
		| currBlock |
		currBlock := blockMapping at: blockName.
		currBlock predecessors removeAll.
		predOrder
			collect: [ :pred |
				blockMapping
					at: pred
					ifAbsent: [ self error: 'Invalid predecessor: ' , pred ] ]
			thenDo: [ :pred | currBlock addPredecessor: pred ] ]
]

{ #category : 'initialization' }
DRIRParser >> initializeFor: inputString [

	| varRegex opcodeRegex operandsRegex |
	input := inputString lines asOrderedCollection
		         collect: #trim
		         thenSelect: #isNotEmpty.
	cfg := DRControlFlowGraph new.
	self initializeOpcodeToInstrMap.

	blockMapping := Dictionary new.
	instrMapping := Dictionary new.
	instrDependencies := Dictionary new.
	
	blockPredecessorsOrder := Dictionary new.

	blockLabelRegex := '^\w[\w\d]*\:(\[[\w\d, ]*\])?$'.

	varRegex := '(\w[\w\d]*)'.
	opcodeRegex := '[^(]+'.
	operandsRegex := '[^)]*'.
	instructionRegex := varRegex , '\:=' , opcodeRegex , '\('
	                    , operandsRegex , '\)'
]

{ #category : 'initialization' }
DRIRParser >> initializeOpcodeToInstrMap [

	opcodeToInstrMap := Dictionary newFrom: {
			                    ('copy' -> DRCopy).

			                    ('if' -> DRBranchIfCondition).
			                    ('jump' -> DRJump).

			                    ('noop' -> DRNoop).

	}.

	DRInstruction allSubclasses
		reject: [ :i | { DRMessageSend. DRMethod. DRGetConditionCode } includes: i ]
		thenDo: [ :i | opcodeToInstrMap at: i new opcode asLowercase put: i ].
		
	DRCondition allSubclasses
		reject: [ :c | c = DRTypeOf ]
		thenDo: [ :c | opcodeToInstrMap at: c new opcode asLowercase put: DRGetConditionCode ].
	
]

{ #category : 'parsing' }
DRIRParser >> parse [

	initialBlock := input removeFirst.
	self
		assert: (initialBlock matchesRegex: '\[\[\w[\w\d]*\]\]')
		description: 'Invalid initial block'.
	initialBlock := (initialBlock allButFirst: 2) allButLast: 2.

	input whileNotEmpty: [ :blockLabel |
		| blockName |
		self
			assert: (blockLabel matchesRegex: blockLabelRegex)
			description: 'Invalid block label' , blockLabel.
		blockName := self parseBlockLabel: blockLabel.
		self parseBlock: blockName.
	].

	self fixDependencies.

	cfg validate.
	^ cfg
]

{ #category : 'parsing' }
DRIRParser >> parseBlock: blockName [

	| b |
	b := (blockName = initialBlock) ifFalse: [ cfg newBasicBlock ]
												ifTrue: [ cfg initialBasicBlock ].

	[ input isNotEmpty and: [
		(input first matchesRegex: blockLabelRegex) not ] ] whileTrue: [
		| instrStr equals result paren opcode operands |
		instrStr := input removeFirst.
		instrStr := instrStr copyReplaceAll: ' ' with: ''.

		self
			assert: (instrStr matchesRegex: instructionRegex)
			description: 'Invalid instruction: ' , instrStr.

		equals := instrStr findFirst: [ :ch | ch = $= ].
		paren := instrStr findFirst: [ :ch | ch = $( ].
		result := instrStr copyFrom: 1 to: equals - 2.
		opcode := instrStr copyFrom: equals + 1 to: paren - 1.
		operands := instrStr copyFrom: paren + 1 to: instrStr size - 1.
		self
			addInstr: opcode
			to: b
			withResult: result
			andOperands: operands ].

	(b instructions size > 1 and: b endInstruction isNoop) ifTrue: [ |previousInstr |
		previousInstr := b instructions at: b instructions size - 1.
		b removeInstruction: previousInstr.
		b endInstruction: previousInstr ].

	blockMapping at: blockName put: b.
	
	^ b
]

{ #category : 'parsing' }
DRIRParser >> parseBlockLabel: blockLabel [

	| label |
	label := blockLabel copyReplaceAll: ' ' with: ''.
	(label includes: $[) ifTrue: [
		| parts blockName |
		parts := label substrings: ':['.
		blockName := parts first.
		blockPredecessorsOrder at: blockName put: (parts second allButLast substrings: ',').
		^ blockName ].

	^ label allButLast
]

{ #category : 'adding' }
DRIRParser >> resolveInstruction: opcode [

	^ opcodeToInstrMap
		  at: opcode asLowercase
		  ifAbsent: [ self error: 'Invalid opcode ' , opcode ]
]
