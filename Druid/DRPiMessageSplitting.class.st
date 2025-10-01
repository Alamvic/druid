Class {
	#name : 'DRPiMessageSplitting',
	#superclass : 'DROptimisation',
	#instVars : [
		'solver',
		'initialPhis'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiMessageSplitting >> applyTo: cfg [

	| anyChanged |
	"Simplify phis before to avoid entering border cases.
	For example, if a we have a cyclic Phi node defined in terms of itself:
	
	  phi1 := phi(phi1, someValue)
	
	"
	(DRPhiSimplication then: DRCleanControlFlow) applyTo: cfg.
	
	solver := self solverFor: cfg.
	
	self doWhilePossible: [
		initialPhis := cfg phiFunctions.
		anyChanged := false.
		cfg blocksDo: [ :b |
			(b piNodes anySatisfy: [ :pi | self trySplitWith: pi ]) ifTrue: [
				anyChanged := true
			]
		].
		anyChanged ifTrue: [ (DRPhiSimplication then: DRCleanControlFlow) applyTo: cfg ].
		anyChanged
	]
]

{ #category : 'accessing' }
DRPiMessageSplitting >> doWhilePossible: aBlock [

	| maxIters continue |
	maxIters := 5.
	continue := true.
	[ maxIters > 0 and: continue ] whileTrue: [
		continue := aBlock value.
		maxIters := maxIters - 1.
	]
]

{ #category : 'accessing' }
DRPiMessageSplitting >> recursivePhiDependenciesOf: aDRValue do: aBlock [

	| worklist visited |
	worklist := OrderedCollection with: aDRValue.
	visited := Set new.

	[ worklist isEmpty ] whileFalse: [
		| current |
		current := worklist removeFirst.
		current isPhiFunction ifTrue: [ aBlock value: current ].
		current operands do: [ :op |
			(visited includes: op) ifFalse: [
				worklist add: op.
				visited add: op ]
		]
	]
]

{ #category : 'accessing' }
DRPiMessageSplitting >> shouldConsiderSplitting: aDRPhiFunction toSplitWith: aDRPiNode [

	"Makes no sense to split if there's only one predecessor"
	aDRPhiFunction operands size = 1 ifTrue: [ ^ false ].
	
	"Ignore blocks that are only phis and an unconditional jump, splitting them will only generate another similar block"
	(aDRPhiFunction basicBlock instructions allSatisfy: [ :i |
		 i isPhiFunction or: [ i isNonConditionalJump ] ]) ifTrue: [
		^ false ].

	"If they're in the same block, splitting will do nothing, at most DBE should delete the branch"
	aDRPhiFunction basicBlock = aDRPiNode basicBlock ifTrue: [ ^ false ].

	"do not split to phis that were added by previous splits"
	(initialPhis includes: aDRPhiFunction) ifFalse: [ ^ false ].

	"if the pi is unsatisfiable, it'll be removed by DBE, no need to split"
	^ solver isSatisfiable: aDRPiNode
]

{ #category : 'accessing' }
DRPiMessageSplitting >> solverFor: cfg [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
DRPiMessageSplitting >> trySplitPhi: aDRPhiFunction toIndex: operandIdx with: aDRPiNode ifSplit: aBlock [

	| op predecessor |
	op := aDRPhiFunction operands at: operandIdx.
	predecessor := aDRPhiFunction basicBlock predecessorAt: operandIdx.

	"Avoid backjumps edges, they are difficult to fix"
	(DREdge
		 withSource: predecessor
		 andDestination: aDRPhiFunction basicBlock) isBackJump ifTrue: [
		^ self ].

	"Evaluate profitability.
	Split if either 
	 - the operands is a setConditionCode instruction (for better code gen)
	 - the branch is not satisfiable for that operand"
	(op isSetConditionCode not and: [
		 solver
			 isSatisfiable: aDRPiNode
			 replacingPhi: aDRPhiFunction
			 for: op ]) ifTrue: [ ^ self ].

	aDRPhiFunction basicBlock splitTo: predecessor.
	aBlock value
]

{ #category : 'accessing' }
DRPiMessageSplitting >> trySplitWith: pi [
	| maxAttemptsPerPi |
	maxAttemptsPerPi := 3.
	self recursivePhiDependenciesOf: pi do: [ :phi |
		(self shouldConsiderSplitting: phi toSplitWith: pi)
			ifTrue: [
				phi operands withIndexDo: [ :phiOp :i |
					self trySplitPhi: phi toIndex: i with: pi ifSplit: [ ^true ]
				].
				(maxAttemptsPerPi := maxAttemptsPerPi - 1) > 0 ifFalse: [ ^false ]
			]
		].

	^ false
]
