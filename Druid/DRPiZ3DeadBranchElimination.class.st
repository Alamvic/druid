Class {
	#name : 'DRPiZ3DeadBranchElimination',
	#superclass : 'DRPiDeadBranchElimination',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'accessing' }
DRPiZ3DeadBranchElimination >> applyTo: cfg [

	solver := DRZ3ConstraintSolver for: cfg.
	super applyTo: cfg.
]
