Class {
	#name : 'DRPiConstantDeadBranchElimination',
	#superclass : 'DRPiDeadBranchElimination',
	#category : 'Druid-Optimizations',
	#package : 'Druid',
	#tag : 'Optimizations'
}

{ #category : 'initialization' }
DRPiConstantDeadBranchElimination >> initialize [

	super initialize.
	solver := DRConstantConstraintSolver new.
]
