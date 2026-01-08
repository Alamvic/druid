Class {
	#name : 'DRPiABCDDeadBranchElimination',
	#superclass : 'DRPiDeadBranchElimination',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'operations' }
DRPiABCDDeadBranchElimination >> applyTo: cfg [

	solver := DRABCDConstraintSolver for: cfg.
	super applyTo: cfg.
]
