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

	solver := DRZ3ConstraintSolver for: cfg.
"	solver := DRConstantConstraintSolver new."

	cfg allConditionalJumps do: [ :condJump |
		| splittable |
		splittable := condJump targets anySatisfy: [ :b |
			              b piNodes anySatisfy: [ :pi |
				              self
					              firstPhiDependencyOf: pi
					              do: [ :phi |
						              (solver isSatisfiable: pi) and: [
							              phi operands anySatisfy: [ :phiOp |
								              (solver
									               isSatisfiable: pi
									               replacingPhi: phi
									               for: phiOp) not ] ] ]
					              ifNone: false
							]
		].
		1 haltIf: splittable
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
