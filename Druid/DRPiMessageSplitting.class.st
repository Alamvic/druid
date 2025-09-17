Class {
	#name : 'DRPiMessageSplitting',
	#superclass : 'DROptimisation',
	#instVars : [
		'solver'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiMessageSplitting >> applyTo: cfg [

	| anyChanged |
	solver := self solverFor: cfg.

	self doWhilePossible: [
		anyChanged := false.
		cfg allConditionalJumps do: [ :condJump |
			condJump targets do: [ :b |
				(b piNodes anySatisfy: [ :pi | self trySplitWith: pi ]) ifTrue: [
					anyChanged := true
				]
			]
		].
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
DRPiMessageSplitting >> firstPhiDependencyOf: aDRValue do: aBlock ifNone: anotherBlock [

	| worklist |
	worklist := OrderedCollection with: aDRValue.
	
	[ worklist isEmpty ] whileFalse: [ 
		|current|
		current := worklist removeFirst.
		current isPhiFunction ifTrue: [ ^aBlock value: current ].
		worklist addAll: current operands.
	].

	^ anotherBlock value
]

{ #category : 'accessing' }
DRPiMessageSplitting >> solverFor: cfg [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
DRPiMessageSplitting >> trySplitWith: pi [

	self
		firstPhiDependencyOf: pi
		do: [ :phi |
			(solver isSatisfiable: pi) and: [
				phi operands do: [ :phiOp |
					(solver isSatisfiable: pi replacingPhi: phi for: phiOp)
						ifFalse: [
							phi basicBlock splitTo: phiOp basicBlock.
							^ true ]
				]
			]
		]
		ifNone: [].
		
	^ false
]
