Class {
	#name : 'DRPiZ3DeadBranchElimination',
	#superclass : 'DRPiDeadBranchElimination',
	#instVars : [
		'signedIntOverflowIsUB'
	],
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'as yet unclassified' }
DRPiZ3DeadBranchElimination class >> withUndefinedBehaviour: ub [

	^ self new withUndefinedBehaviour: ub
]

{ #category : 'accessing' }
DRPiZ3DeadBranchElimination >> applyTo: cfg [

	solver := DRZ3ConstraintSolver
		          for: cfg
		          signedIntOverflowIsUB:
		          (signedIntOverflowIsUB ifNil: [ true ]).
	super applyTo: cfg
]

{ #category : 'as yet unclassified' }
DRPiZ3DeadBranchElimination >> withUndefinedBehaviour: ub [

	signedIntOverflowIsUB := ub
]
