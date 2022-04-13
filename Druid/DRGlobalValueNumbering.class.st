Class {
	#name : #DRGlobalValueNumbering,
	#superclass : #Object,
	#instVars : [
		'dominatorTree',
		'valueNumbers',
		'hashTables',
		'currentScope'
	],
	#category : #'Druid-Optimizations'
}

{ #category : #operations }
DRGlobalValueNumbering >> adjustPhiFunctions: block [

	block phiFunctions do: [ :instruction |
		instruction operands: (instruction operands collect: [ :operand | self valueNumberOf: operand])
	]
]

{ #category : #operations }
DRGlobalValueNumbering >> applyTo: cfg [

	dominatorTree := DRDominatorTree new.
	dominatorTree dominatorTreeOf: cfg.
	hashTables := OrderedCollection new.
	currentScope := 0.
	valueNumbers := Dictionary new.
	
	self dominatorBasedValueNumbering: dominatorTree tree.
]

{ #category : #operations }
DRGlobalValueNumbering >> dominatorBasedValueNumbering: node [

	| symbolTable |
	symbolTable := self initializeScope.
	self processPhiFunctions: node block.
	self processInstructions: node block.
	node children do: [ :child |
		self adjustPhiFunctions: child block.
	].
	
	node children do: [ :child |
		self dominatorBasedValueNumbering: child
	].
	self finalizeScope
]

{ #category : #operations }
DRGlobalValueNumbering >> finalizeScope [
	
	currentScope := currentScope - 1.
	hashTables removeLast.
]

{ #category : #operations }
DRGlobalValueNumbering >> initializeScope [
	
	| symbolTable |
	currentScope := currentScope + 1.
	symbolTable := Dictionary new.
	^ hashTables addLast: symbolTable.
]

{ #category : #operations }
DRGlobalValueNumbering >> processInstructions: block [

	| expression |

	block instructions copy do: [ :instruction |
		instruction isPhiFunction ifFalse: [
			instruction operands:
			(instruction operands collect: [ :operand | self valueNumberOf: operand]).
			expression := instruction prettyPrint.
			expression := expression copyFrom: (expression indexOf: $=) + 2 to: expression size.
			self searchHashTables: expression ifPresent: [ :valueNumber |
				valueNumbers at: instruction put: valueNumber.
				block removeInstruction: instruction.
			]
			ifAbsent: [
				valueNumbers at: instruction put: instruction.
				(hashTables at: currentScope) at: expression put: instruction.
			].

		]
	]
]

{ #category : #operations }
DRGlobalValueNumbering >> processMeaningless: instruction in: block [

	instruction operands doWithIndex: [ :operand :index |
		(instruction operands copyFrom: index + 1 to: instruction operands size) do: [ :other |
			((self valueNumberOf: operand) = self valueNumberOf: other)
			ifFalse: [ ^ false ].
		].
	].
	
	valueNumbers at: instruction put: instruction operand1.
	block removeInstruction: instruction.
	
	^ true
]

{ #category : #operations }
DRGlobalValueNumbering >> processPhiFunctions: block [

	block phiFunctions copy do: [ :instruction |
		((self processMeaningless: instruction in: block)
		ifFalse: [ self processRedundant: instruction in: block ])
		ifFalse: [
			valueNumbers at: instruction put: instruction.
			(hashTables at: currentScope) at: instruction put: instruction.
		]
	]
]

{ #category : #operations }
DRGlobalValueNumbering >> processRedundant: instruction in: block [

	| isRedundant |
	
	block phiFunctions copy do: [ :other |
		other = instruction ifFalse: [
			isRedundant := true.
			instruction operands with: other operands do: [ :instructionOperand :otherOperand |
				((self valueNumberOf: otherOperand) = self valueNumberOf: instructionOperand)
				ifFalse: [ isRedundant := false ].
			].
			isRedundant ifTrue: [
				valueNumbers at: instruction put: other.
				block removeInstruction: instruction.
				^ true
			]
		]
	].

	^ false
]

{ #category : #operations }
DRGlobalValueNumbering >> searchHashTables: expression ifPresent: aPresentBlock ifAbsent: anAbsentBlock [

	hashTables reverseDo: [ :table |
		table at: expression ifPresent: [ :valueNumber | aPresentBlock cull: valueNumber ]
	].
	
	anAbsentBlock value
]

{ #category : #operations }
DRGlobalValueNumbering >> valueNumberOf: expr [
	
	^ valueNumbers at: expr ifAbsent: [ expr ]
]
