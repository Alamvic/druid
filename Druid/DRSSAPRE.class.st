Class {
	#name : #DRSSAPRE,
	#superclass : #DRControlFlowOptimisation,
	#category : #'Druid-Optimizations'
}

{ #category : #'as yet unclassified' }
DRSSAPRE >> checkForRedundancyCrossBlocks: aDRControlFlowGraphForTesting [

	| instructionList instruction1 instruction2 |
	instructionList := OrderedCollection new.
	aDRControlFlowGraphForTesting blocks do: [ :aBlock | 
		aBlock instructions do: [ :i | (i value isJump ) ifFalse:[ instructionList add: i ]]].
	instruction1 := instructionList at: 1.
	instruction2 := instructionList at: 2.
	(instruction1 hasSameExpressionAs: instruction2) ifTrue: [ instruction2 replaceOperandAtIndex: 1 by: instruction1 ]
	
]

{ #category : #'as yet unclassified' }
DRSSAPRE >> checkForRedundancyWithinBlock: aDRBasicBlock [

	| instruction1 instruction2 |
	instruction1 := aDRBasicBlock instructions at: 1.
	instruction2 := aDRBasicBlock instructions at: 2.

	(instruction1 hasSameExpressionAs: instruction2) ifTrue: [ 
		instruction2 replaceOperandAtIndex: 1 by: instruction1 ]
]

{ #category : #instruction }
DRSSAPRE >> compareConstantExpression: e1 and: e2 [

	| applyRedundancy |
	(e1 isEqualTo: e2)
		ifTrue: [ applyRedundancy ]
		ifFalse: [ ^ self ]
]

{ #category : #accessing }
DRSSAPRE >> doApply: cfg [

	cfg blocks size < 2
		ifTrue: [ 
			cfg blocks do: [ :aBlock | 
				self checkForRedundancyWithinBlock: aBlock ] ]
		ifFalse: [ self checkForRedundancyCrossBlocks: cfg ]
]

{ #category : #initialization }
DRSSAPRE >> initialize [

	super initialize.
	
]

{ #category : #'as yet unclassified' }
DRSSAPRE >> nameLater: cfg [
"
a blocklist for collecting blocks
an expression list for expression in each block
left operand list for each expression of a block
right operand list for each expression of a block
compare first the left operand list if same var 
then compare right operands if not same
return ".
"	blocklist := OrderedCollection new.
	expressionlist := OrderedCollection new.
	[ :e | e in cfg  ]
	for each in cfg: add block to blocklist
	for each in blocklist: 
		for expression in each:
			add right operand to rightlist
	
	compare"
]
