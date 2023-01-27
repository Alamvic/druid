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
DRSSAPRE >> checkForRedundancyCrossBranch: cfg [

	| instructionList instruction1 instruction2 afterReplacedList phiList |
	instructionList := OrderedCollection new.
	afterReplacedList := OrderedCollection new.
	phiList := OrderedCollection new.

	cfg blocksInReversePostOrderDo: [ :b | 
		b instructions collect: [ :i | 
			(i value isJump or: i value isPhiFunction) ifFalse: [ 
				instructionList add: i ].
			i value isPhiFunction ifTrue: [ phiList add: i ] ] ].

	[ instructionList size > 2 ] whileTrue: [ 
		instruction1 := instructionList at: 1.
		instruction2 := instructionList at: 2.
		(instruction1 hasSameExpressionAs: instruction2) ifTrue: [ 
			instruction2 replaceOperandAtIndex: 1 by: instruction1.
			afterReplacedList add: instruction2.
			instructionList remove: instruction2 ] ].
	((afterReplacedList at: 1) hasSameExpressionAs:
		 (afterReplacedList at: 2)) ifTrue: [ 
		(phiList at: 1) replaceOperandAtIndex: 1 by: (instructionList at: 1).
		(phiList at: 1) replaceOperandAtIndex: 2 by: (instructionList at: 1) ]
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

	self lookForRedundancy: cfg.
]

{ #category : #initialization }
DRSSAPRE >> initialize [

	super initialize.
	
]

{ #category : #'as yet unclassified' }
DRSSAPRE >> lookForRedundancy: cfg [

	| instruction |
	instruction := cfg blocks removeFirst instructions at: 1.
	
	cfg blocksInReversePostOrderDo: 
	[ :b | (b = cfg firstBasicBlock) ifTrue: 
		[b instructions allButFirstDo:
			[ :i | ((i isJump not and: [ i isNoop not ]) and: [ i hasSameExpressionAs: instruction ]) 
				ifTrue: [ i replaceOperandAtIndex: 1 by: instruction ]	
			]
		]
		    ifFalse:[ b instructions do:
			[ :i | ((i isJump not and: [ i isNoop not ]) and: [ i hasSameExpressionAs: instruction ]) 
				ifTrue: [ i replaceOperandAtIndex: 1 by: instruction ]	
			]]
	] 
]
